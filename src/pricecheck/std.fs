namespace std


open System
open System.Text.Json
open System.Net.Http
open System.Collections.Generic


type StringPair = ValueTuple<string, string>


[<AutoOpen>]
module Structure =

    let pair<'a, 'b> (a: 'a) (b: 'b) =
        struct (a, b)

    let disregard<'a> a = ignore<'a> a


module Env =

    let argv () =
        Environment.GetCommandLineArgs()

    let exit code =
        Environment.Exit(code)

    let var name =
        let envVar = Environment.GetEnvironmentVariable(name)
        if envVar |> isNull then
            ValueNone
        else 
            ValueSome envVar

module Encoding =

    let utf8Str (bytes: ReadOnlySpan<byte>) =
        System.Text.Encoding.UTF8.GetString(bytes)

    let utf8Bytes (str: string) =
        System.Text.Encoding.UTF8.GetBytes(str)

type HttpRequest =
    { Method: string
      Uri: Uri
      Proxy: Uri ValueOption
      Headers: StringPair seq
      Content: ReadOnlyMemory<byte> }

type HttpResponse =
    { StatusCode: Net.HttpStatusCode
      Headers: StringPair list
      Content: ReadOnlyMemory<byte> }


module Json =
    let deserialize<'a> (json: string) =
        JsonSerializer.Deserialize<'a>(json)


module Http =

    open System.Net

    module HttpUtils =

        open System.Text

        let [<Literal>] private UnreservedHttpChars =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~"

        let urlEncode (text: string) =
            let sb = StringBuilder(text.Length + 255)
            for ch in text do
                if ch = ' ' then
                    sb.Append('+')
                    |> ignore<StringBuilder>
                else if UnreservedHttpChars.IndexOf(ch) = -1 then
                    sb.AppendFormat("%{0:X2}", int ch)
                    |> ignore<StringBuilder>
                else
                    sb.Append(ch) |> ignore<StringBuilder>
            sb.ToString()

        let urlEncodePair (pair: StringPair) =
            let struct (a, b) = pair
            $"{urlEncode a}={urlEncode b}"

        let urlEncodeSeq (seq: StringPair seq) =
            String.concat "&" 
            <| Seq.map urlEncodePair seq

    module HttpRequest =
        let construct method uri =
            { Method=method;
              Uri=Uri(uri);
              Proxy=ValueNone;
              Headers=[];
              Content=ReadOnlyMemory<byte>() }

        let proxy proxy req =
            match proxy with
            | "" -> { req with Proxy=ValueNone }
            | _  -> { req with Proxy=ValueSome <| Uri(proxy) }

        let content (content: byte[]) (req: HttpRequest) =
            { req with Content=ReadOnlyMemory<byte>(content) }

        let contentStr (content: string) (req: HttpRequest) =
            { req with Content=ReadOnlyMemory<byte>(Encoding.utf8Bytes content) }


        let internal toReqMsg (req: HttpRequest) =
            let reqMsg = new HttpRequestMessage(
                HttpMethod.Parse(req.Method),
                req.Uri,
                Content=if req.Content.IsEmpty then null else  new ReadOnlyMemoryContent(req.Content)
            )

            for (name, value) in req.Headers do
                if not <| reqMsg.Headers.TryAddWithoutValidation(name, value) then
                    if not <| reqMsg.Content.Headers.TryAddWithoutValidation(name, value) then
                        invalidOp<unit> $"failed to add header {name} {value}"

            reqMsg


    let internal httpRespOfRespMsg (content: byte[]) (response: HttpResponseMessage) =
        let headers =
            Seq.collect (
                    fun (header: KeyValuePair<string, string seq>) ->
                        Seq.map (
                            fun value ->
                                struct (header.Key, value)
                        ) header.Value
                )
                response.Headers
            |> List.ofSeq
        { StatusCode=response.StatusCode
          Headers=headers
          Content=ReadOnlyMemory<byte>(content) }

    module Http =
    
        let req method uri = HttpRequest.construct method uri

        let headers (headers: StringPair seq) (req: HttpRequest) =
            { req with Headers=headers}

        let content (content: ReadOnlyMemory<byte>) (req: HttpRequest) =
            { req with Content=content }

        let str (response: HttpResponse) =
            Encoding.utf8Str response.Content.Span

        let retrieve (req: HttpRequest) = task {
            use handler = new HttpClientHandler()

#if DEBUG
            handler.ServerCertificateCustomValidationCallback <- fun _ _ _ _ -> true
#endif

            match req.Proxy with
            | ValueSome proxy -> handler.Proxy <- WebProxy(proxy)
            | ValueNone -> ()

#if DEBUG
            handler.Proxy <- WebProxy("127.0.0.1:8888")
#endif

            use client = new HttpClient(handler)
            use req = HttpRequest.toReqMsg req
            use! resp = client.SendAsync(req)
            resp.EnsureSuccessStatusCode()
            |> disregard<HttpResponseMessage>
            use content = resp.Content
            let! content = content.ReadAsByteArrayAsync()
            let response = httpRespOfRespMsg content resp
            return response
        }
