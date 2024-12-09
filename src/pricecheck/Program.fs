open PriceCheck.Lib
open System.Text.Json



        
let head () = task {
    let zipcode = "32413"
    let storeId = "Publix #1005 - Ocean Park Pavilion"
    let! session = Publix.createSession zipcode storeId
    let! items = Publix.search "pasta sauce" session
    let! items = Publix.items items session
    let filteredItems = 
        Seq.distinctBy (fun (item: PublixItemDetailed) -> item.BrandName.Trim()) items
        |> Array.ofSeq
        |> Array.map PublixItem.ofDetailedItem

    (* printfn "%A" filteredItems *)

    let itemsOnSale =
        Publix.filterOnSale items
        |> Seq.map PublixItem.ofDetailedItem
        |> Seq.filter (fun item -> not <| item.Sale.Contains("Spend"))
        |> Seq.distinctBy (fun item -> struct (item.Brand.Trim(), item.Sale.Trim()))
        |> Array.ofSeq

    printfn "%A" itemsOnSale
    return ()
}

head().GetAwaiter().GetResult()