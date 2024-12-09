namespace PriceCheck.Lib

open System.Text.Json

type ShopItem =
    { Name: string
      Size: string
      Cost: decimal }

[<AutoOpen>]
module internal Shared =
    let jsonOptions = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

