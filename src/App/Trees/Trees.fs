module App.Trees.Trees

type FileSystemItem =
    | File of FileInfo
    | Directory of DirectoryInfo
and FileInfo = {name: string; fileSize: int}
and DirectoryInfo = {name: string; dirSize: int; subItems: FileSystemItem list}

type Tree<'LeafData, 'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq
    
module Tree =
    let rec cata fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = cata fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            fNode nodeInfo (subtrees |> Seq.map recurse)
            
    let rec fold fLeaf fNode acc (tree) : 'r =
        let recurse = fold fLeaf fNode
        printfn "Trees %A " tree
        match tree with
        | LeafNode leafInfo ->
            fLeaf acc leafInfo
        | InternalNode(nodeInfo, subtrees) ->
            let localAccum = fNode acc nodeInfo
            let finalAccum = subtrees |> Seq.fold recurse localAccum
            finalAccum
            
    let rec map fLeaf fNode tree =
        let recurse = map fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo
        | InternalNode(nodeInfo, subtrees) ->
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = subtrees |> Seq.map recurse
            InternalNode(newNodeInfo, newSubtrees)
            
    let rec iter fLeaf fNode tree =
        let recurse = iter fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            fLeaf leafInfo
        | InternalNode(nodeInfo, subtrees) ->
            subtrees |> Seq.iter recurse
            fNode nodeInfo
            
//let fromFile fileInfo = LeafNode fileInfo
//let fromDir dirInfo subItems = InternalNode (dirInfo, subItems)
//
//
//let readme = fromFile {name="readme.txt"; fileSize=1}
//let config = fromFile {name="config.xml"; fileSize=2}
//let build  = fromFile {name="build.bat"; fileSize=3}
//let src = fromDir {name="src"; dirSize=10; subItems = []} [readme; config; build]
//let bin = fromDir {name="bin"; dirSize=10; subItems = []} []
//let root = fromDir {name="root"; dirSize=5; subItems = []} [src; src; bin;]

//let totalSize fileSystemItem =
//    let fFile acc (file: FileInfo) =
//        printfn "File:%A" file.name
//        acc + file.fileSize
//    let fDir acc dir =
//        printfn "Dir: %A" dir.name
//        acc + dir.dirSize
//    Tree.fold fFile fDir 0 fileSystemItem
//
//let largestFile fileSystemItem =
//    let fFile (largestSoFarOpt: FileInfo option) (file: FileInfo) =
//        match largestSoFarOpt with
//        | None ->
//            Some file
//        | Some largestSoFar ->
//            if largestSoFar.fileSize > file.fileSize then
//                Some largestSoFar
//            else
//                Some file
//    
//    let fDir largestSoFarOpt dirInfo =
//        largestSoFarOpt
//    
//    Tree.fold fFile fDir None fileSystemItem
//    
//let largestDir fileSystemItem =
//    let fFile largestSoFarOpt file =
//        largestSoFarOpt
//    let fDir (largestSoFarOpt: DirectoryInfo option) (dir: DirectoryInfo) =
//        match largestSoFarOpt with
//        | None -> Some dir
//        | Some largestSoFar ->
//            if largestSoFar.dirSize > dir.dirSize then
//                Some largestSoFar
//            else
//                Some dir
//    Tree.fold fFile fDir None fileSystemItem
//
//readme |> largestDir

type FileSystemTree = Tree<System.IO.FileInfo, System.IO.DirectoryInfo>

let fromFile (fileInfo: System.IO.FileInfo) =
    LeafNode fileInfo

let rec fromDir (dirInfo: System.IO.DirectoryInfo) =
    let subItems = seq {
        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
    }
    InternalNode (dirInfo, subItems)


let totalSize fileSystemItem =
    let fFile acc (file: System.IO.FileInfo) =
        printfn "File:%A" file.Name
        acc + file.Length
    let fDir acc (dir: System.IO.DirectoryInfo) =
        printfn "Dir: %A" dir.Name
        acc
    Tree.fold fFile fDir 0 fileSystemItem
    
let largestFile fileSystemItem =
    let fFile (acc: System.IO.FileInfo option) (file: System.IO.FileInfo) =
        match acc with
        | None -> Some file
        | Some largestFile ->
            if largestFile.Length > file.Length then
                Some largestFile
            else
                Some file
    
    let fDir acc dir =
        acc
    Tree.fold fFile fDir None fileSystemItem

let currentDir = fromDir(System.IO.DirectoryInfo("."))

currentDir |> largestFile

let dirListing fileSystemItem =
    let printDate d = d.ToString()
    let mapFile (fi: System.IO.FileInfo) =
        sprintf "%10i  %s  %-s" fi.Length (printDate fi.LastWriteTime) fi.Name
    let mapDir (di: System.IO.DirectoryInfo) =
        di.FullName
    Tree.map mapFile mapDir fileSystemItem

currentDir |> dirListing |> Tree.iter (printfn "%s") (printfn "\n%s")