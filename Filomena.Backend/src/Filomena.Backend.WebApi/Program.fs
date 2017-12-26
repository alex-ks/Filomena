// Learn more about F# at http://fsharp.org

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

module Program = 
    let index = """
    <html>
        <head>
            <title>Hello, Suave!</title>
        </head>
        <body>
            <p>
                <a href="/hello">Hello!</a>
            </p>
            <p>
                <a href="/goodbye">Good bye!</a>
            </p>
        </body>
    </html>
"""

    let app = 
        choose
            [ GET >=> choose 
                [ path "/" >=> OK index
                  path "/hello" >=> OK "Hello, GET"
                  path "/goodbye" >=> OK "Good bye, GET" ]
              POST >=> choose
                [ path "/hello" >=> OK "Hello POST"
                  path "/goodbye" >=> OK "Good bye POST" ] ]

    let config = defaultConfig
    

    [<EntryPoint>]
    let main _ =
        do startWebServer defaultConfig app
        0    
