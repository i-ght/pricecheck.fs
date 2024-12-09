namespace PriceCheck.Lib

open System
open System.Text.Json
open System.Threading.Tasks

open std
open std.Http

type PublixStore =
    { Id: string
      Name: string
      Lat: double
      Lon: double }

type PublixSession =
    { Zipcode: string
      StoreId: string
      InstacartSid: string; 
      Cookies: string
      ZoneId: string 
      Store: PublixStore
      InventoryToken: string
      InventoryStoreId: string }

(* PublixStores *)
type PublixLocationName = { 
    LocationNameString: string 
}

type PublixLocationCoordinates = {
    Latitude: double
    Longitude: double
}

type PublixLocationDetails = {
    Coordinates: PublixLocationCoordinates
    RetailerLocationId: string
    ViewSection: PublixLocationName
}

type PublixPickupRetailer = {
    Locations: PublixLocationDetails []
}

type PublixPickupRetailers ={
    PickupRetailers: PublixPickupRetailer []
}

type PublixStores = {
    AvailablePickupRetailerServices: PublixPickupRetailers
}

(* UpdateUserLocation *)

type PublixUpdateUserLocation = {
    ZoneId: string
}

type PublixZone = {
    UpdateUserLocation: PublixUpdateUserLocation
}

(* Search *)

type PublixSearchItems = {
    ItemGridId: string
    ItemIds: string []
    __typename: string
}

type PublixSearchContentPlacement = {
    Content: PublixSearchItems
    __typename: string
}

type PublixSearchResultsPlacements = {
    SearchId: string
    Placements: PublixSearchContentPlacement []
}

type PublixSearchResults = {
    SearchResultsPlacements: PublixSearchResultsPlacements
}

(* Inventory Session *)

type PublixShop = {
    Id: string
    RetailerInventorySessionToken: string
    RetailerLocationId: string
    ServiceType: string
}

type PublixShops = {
    Shops: PublixShop []
}

type PublixShopData = {
    ShopCollection: PublixShops
}

(* items *)

type PublixItemBadge = {
    GenericSaleLabelString: string
    ActionLabelString: string
    BadgeVariant: string
    OfferLabelString: string
    ApplyCouponString: string
    CouponClippedVariant: string
    MultiUnitClippablePromotionVariant: string
    OfferSublabelString: string
    OfferReferenceString: string
}

type PublixItemCard = {
    Id: string
    FullPriceString: string
    PriceAriaLabelString: string
    PricePerUnitString: string
    PriceString: string
    PricingUnitSecondaryString: string
    PricingUnitString: string
    DiscountHeaderString: string
    DiscountSubHeaderString: string
    PriceSuffixString: string
    PlainFullPriceString: string
    PriceScreenReaderString: string
    FullPriceScreenReaderString: string
}

type PublixItemDetails = {
    DealAppliedLabelString: string
    DealAppliedSoFarMessageString: string
    FullPriceString: string
    MixAndMatchItemsTitleString: string
    MixAndMatchItemsVariant: string
    PriceAriaLabelString: string
    PriceEstimateVariant: string
    PricePerUnitString: string
    PriceString: string
    PricingUnitSecondaryString: string
    PricingUnitString: string
    SaleDisclaimerString: string
    LoyaltyProgramVariant: string
}

[<CLIMutable>]
type PublixViewSectionPrice = {
    Badge: PublixItemBadge
    ItemCard: PublixItemCard
    ItemDetails: PublixItemDetails
    FullPriceString: string
    PriceString: string
}

type PublixPrice = {
    Id: string
    ItemId: string
    ViewSection: PublixViewSectionPrice
}

type PublixItemDetailed = {
    Id: string
    Name: string
    Size: string
    ProductId: string
    BrandName: string
    Price: PublixPrice
}

type PublixItems = {
    Items: PublixItemDetailed []
}

type PublixApiData<'a> = {
    Data: 'a
}

type PublixItem = {
    Id: string
    Name: string
    Brand: string
    Size: string
    Price: string
    Sale: string
}

module PublixItem =
    let ofDetailedItem(detailedItem: PublixItemDetailed) = {
        Id=detailedItem.Id
        Name=detailedItem.Name
        Brand=detailedItem.BrandName
        Size=detailedItem.Size
        Price=detailedItem.Price.ViewSection.PriceString
        Sale=if box detailedItem.Price.ViewSection.Badge = null then "" else detailedItem.Price.ViewSection.Badge.OfferLabelString
    }

module internal PublixInternal =
    open System.Text.RegularExpressions

    let tryFindInstacartSid (headers: StringPair seq) =
        Seq.filter (fun (struct (name, _)) -> name="Set-Cookie") headers
        |> Seq.map (fun (struct (_, value)) -> value)
        |> Seq.tryFind(fun value -> value.Contains("__Host-instacart_sid="))

    let getInstacartSid () = task {
#if DEBUG
        return struct ("v2.37557ebf932d44.CXMc3QTOlL3jtiEG_n2KtEiQMlgiyL6DUAceQunqQ1o", "__Host-instacart_sid=v2.37557ebf932d44.CXMc3QTOlL3jtiEG_n2KtEiQMlgiyL6DUAceQunqQ1o")
#else
        let uri = "https://delivery.publix.com/store/publix/storefront"
        let req = HttpRequest.construct "GET" uri
        let! resp = Http.retrieve req
        let headers = resp.Headers

        let cookieData =
            match tryFindInstacartSid headers with
            | None -> invalidOp "failed to find instacard sid cookie"
            | Some cookieData -> cookieData

        let split = cookieData.Split(';')
        let formattedCookie = split[0]
        let split = formattedCookie.Split('=')
        let struct (name, value) = (split[0], split[1])
        if name <> "__Host-instacart_sid" then
            return invalidOp "error parsing instacart_sid"
        else 
            return struct (value, formattedCookie)
#endif
    }

    let updateLocation (zipcode: string) (cookies: string) = task {
        let uri = "https://delivery.publix.com/graphql"

        let headers = [
            struct ("Cookie", cookies)
            struct ("Origin", "https://delivery.publix.com")
            struct ("Referer", "https://delivery.publix.com/store/publix/storefront")
            struct ("Content-Type", "application/json")
        ]

        let jsonContent = $"""{{"operationName":"UpdateUserLocation","variables":{{"postalCode":"{zipcode}"}},"extensions":{{"persistedQuery":{{"version":1,"sha256Hash":"026db6726eb53a0e36f0b1368de6c274c15105ec0ae94a5ae73568b533016801"}}}}}}"""
        let jsonContent =
            ReadOnlyMemory<byte>(Encoding.utf8Bytes jsonContent)
        let req =
            HttpRequest.construct "POST" uri
            |> Http.headers headers
            |> Http.content jsonContent
        let! resp = Http.retrieve req
        let json = Encoding.utf8Str resp.Content.Span
        let data =
            JsonSerializer.Deserialize<PublixApiData<PublixZone>>(
                json,
                jsonOptions
            )
        return data.Data.UpdateUserLocation.ZoneId
    }

    let stores (zipcode: string) (cookies: string) = task {
        let uri = "https://delivery.publix.com/graphql"
        let query = [
            struct ("operationName", "AvailablePickupRetailerServices")
            struct ("variables", $"""{{"postalCode":"{zipcode}","coordinates":null}}""")
            struct ("extensions", """{"persistedQuery":{"version":1,"sha256Hash":"6bc1dfbf68880694e4acc9b4d53993e1d62870ebc5c2e456e2a80f836ee0ddd5"}}""")
        ]
        let uri =
            $"{uri}?{HttpUtils.urlEncodeSeq query}"

        let req =
            HttpRequest.construct "GET" uri
            |> Http.headers [("Cookie", cookies)]
        let! resp = Http.retrieve req
        let json = Encoding.utf8Str resp.Content.Span
        let data =
            JsonSerializer.Deserialize<PublixApiData<PublixStores>>(
                json,
                jsonOptions
            )
        
        let stores =
            data.Data.AvailablePickupRetailerServices.PickupRetailers[0].Locations
            |> Array.map (fun info -> 
                { Id=info.RetailerLocationId; Name=info.ViewSection.LocationNameString; Lat=info.Coordinates.Latitude; Lon=info.Coordinates.Longitude })
        return stores
    }

    let getInventoryInfo (cookies: string) (zipcode: string) (store: PublixStore) = task {
        let uri = "https://delivery.publix.com/graphql"
        let query = [
            struct ("operationName", """ShopCollection""")
            "variables", $"""{{"retailerSlugs":["publix"],"postalCode":"{zipcode}","coordinates":{{"latitude":{store.Lat},"longitude":{store.Lon}}},"addressId":null,"allowCanonicalFallback":false}}"""
            "extensions", """{"persistedQuery":{"version":1,"sha256Hash":"70e039b105c73d00bd34137e66f7ea1ffe3f207d8442fd203e29e87dbd3d8577"}}"""
        ]
        let uri =
            $"{uri}?{HttpUtils.urlEncodeSeq query}"

        let req =
            HttpRequest.construct "GET" uri
            |> Http.headers [("Cookie", cookies)]
        let! resp = Http.retrieve req

        let json = Encoding.utf8Str resp.Content.Span
        let data =
            JsonSerializer.Deserialize<PublixApiData<PublixShopData>>(
                json,
                jsonOptions
            )

        let shops = data.Data.ShopCollection.Shops
        let shop =
            match Array.tryFind (fun shop -> shop.ServiceType = "pickup") shops with
            | None -> invalidOp $"didn't find inventory info for {store.Id}"
            | Some info -> info

        return shop
    }

    let createSession (zipcode: string) (storeId: string) = task {

        let! (instacardSidCookieValue, instacardSidCookieHeader) =
            getInstacartSid ()

        let! zoneId = updateLocation zipcode instacardSidCookieHeader

        let! stores = stores zipcode instacardSidCookieHeader

        let store =
            match Seq.tryFind (fun (store: PublixStore) -> store.Name = storeId) stores with
            | None -> invalidOp $"did not find store {storeId}"
            | Some store -> store

        let! inventoryInfo = getInventoryInfo instacardSidCookieHeader zipcode store

        let sessionInfo: PublixSession = 
            { Zipcode=zipcode
              StoreId=storeId
              InstacartSid=instacardSidCookieValue
              Cookies=instacardSidCookieHeader
              ZoneId=zoneId
              Store=store
              InventoryToken=inventoryInfo.RetailerInventorySessionToken
              InventoryStoreId=inventoryInfo.Id }
        
        return sessionInfo
    }

    let private resultsRegex = Regex("(\d+) results", RegexOptions.Compiled)

    let search (terms: string) (session: PublixSession) = task {
        let uri = "https://delivery.publix.com/graphql"
        let query = [
            struct ("operationName", "SearchResultsPlacements")
            "variables", $"""{{"filters":[],"action":null,"query":"{terms}","pageViewId":"{Guid.NewGuid()}","retailerInventorySessionToken":"{session.InventoryToken}","elevatedProductId":null,"searchSource":"search","disableReformulation":false,"disableLlm":false,"forceInspiration":false,"orderBy":"bestMatch","clusterId":null,"includeDebugInfo":false,"clusteringStrategy":null,"contentManagementSearchParams":{{"itemGridColumnCount":8}},"shopId":"{session.InventoryStoreId}","postalCode":"{session.Zipcode}","zoneId":"{session.ZoneId}","first":4}}"""
            "extensions", """{"persistedQuery":{"version":1,"sha256Hash":"ccb33e7c6f732eac36557cd077b43325779c901f22ae69169f5d3c0d687eeca7"}}"""
        ]
        let uri =
            $"{uri}?{HttpUtils.urlEncodeSeq query}"

        let cookies = session.Cookies
        let req =
            HttpRequest.construct "GET" uri
            |> Http.headers [("Cookie", cookies)]
        let! resp = Http.retrieve req
        let json = Encoding.utf8Str resp.Content.Span
        let resultCountMatch = resultsRegex.Match(json)
        if not <| resultCountMatch.Success then
            invalidOp "failed to get result count"
        let resultCount = int resultCountMatch.Groups[1].Value
        let data =
            JsonSerializer.Deserialize<PublixApiData<PublixSearchResults>>(
                json,
                jsonOptions
            )
        let results =
            Array.filter (
                fun (item: PublixSearchContentPlacement) -> item.Content.ItemGridId <> null
            ) data.Data.SearchResultsPlacements.Placements
        let results = 
            Seq.map (fun items -> items.Content.ItemIds) results
            |> Seq.concat
        return 
            results 
            |> Seq.chunkBySize 64
    }

    let items (itemIds: string array seq) (session: PublixSession) = task {
        let uri = "https://delivery.publix.com/graphql"

        let items = ResizeArray()
        for itemIds in itemIds do
            let itemsJson = JsonSerializer.Serialize(itemIds)
            let query = [
                struct ("operationName", """Items""")
                "variables", $"""{{"ids":{itemsJson},"shopId":"{session.InventoryStoreId}","zoneId":"{session.ZoneId}","postalCode":"{session.Zipcode}"}}"""
                "extensions", """{"persistedQuery":{"version":1,"sha256Hash":"cfae0717246fddaf0212c9d9cb9fba06d43ff153a7dbd72695f3721b0b578be2"}}"""
            ]
            let uri =
                $"{uri}?{HttpUtils.urlEncodeSeq query}"

            let cookies = session.Cookies
            let req =
                HttpRequest.construct "GET" uri
                |> Http.headers [("Cookie", cookies)]
            let! resp = Http.retrieve req
            let json = Encoding.utf8Str resp.Content.Span
            let data =
                JsonSerializer.Deserialize<PublixApiData<PublixItems>>(
                    json,
                    jsonOptions
                )
            items.AddRange(data.Data.Items)
        return items
    }

    let bogos (session: PublixSession) = task {
        ()
    }

module Publix =
    
    let createSession (zipcode: string) (storeId: string) = 
        PublixInternal.createSession zipcode storeId

    let search (terms: string) (session: PublixSession) =
        PublixInternal.search terms session

    let items (items: string array seq) (session: PublixSession) =
        PublixInternal.items items session

    let filterOnSale (items: PublixItemDetailed seq) =
        let filter (item: PublixItemDetailed) =
            box item.Price.ViewSection.Badge <> null
        Seq.filter filter items