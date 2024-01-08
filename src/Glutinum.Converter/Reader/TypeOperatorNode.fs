module Glutinum.Converter.Reader.TypeOperatorNode

open Glutinum.Converter.GlueAST
open Glutinum.Converter.Reader.Types
open TypeScript
open Fable.Core.JsInterop

let readTypeOperatorNode
    (reader: TypeScriptReader)
    (node: Ts.TypeOperatorNode)
    =

    match node.operator with
    | Ts.SyntaxKind.KeyOfKeyword ->
        if ts.isTypeReferenceNode node.``type`` then
            let typeReferenceNode = node.``type`` :?> Ts.TypeReferenceNode

            // TODO: Remove unboxing
            let symbolOpt =
                reader.checker.getSymbolAtLocation !!typeReferenceNode.typeName

            match symbolOpt with
            | None ->
                failwith (
                    Utils.generateReaderError
                        "type operator (keyof)"
                        "Missing symbol"
                        node
                )

            | Some symbol ->
                match symbol.declarations with
                | Some declarations ->
                    let interfaceDeclaration =
                        declarations[0] :?> Ts.InterfaceDeclaration

                    reader.ReadInterfaceDeclaration interfaceDeclaration
                    |> GlueType.KeyOf

                | None ->
                    failwith (
                        Utils.generateReaderError
                            "type operator (keyof)"
                            "Missing declarations"
                            node
                    )

        else
            failwith (
                Utils.generateReaderError
                    "type operator (keyof)"
                    $"Was expecting a type reference instead got a Node of type %A{node.``type``.kind}"
                    node
            )

    | _ ->
        failwith (
            Utils.generateReaderError
                "type operator"
                $"Unsupported operator %A{node.operator}"
                node
        )
