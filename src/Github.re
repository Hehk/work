open Lwt;
open Cohttp;
open Cohttp_lwt_unix;
open Yojson.Basic;

let headers =
  Header.init()
  |> Header.add(_, "Authorization", "Bearer " ++ Env.githubAccessToken);
let uri = Uri.of_string("https://api.github.com/graphql");

// Graphql responses are of the form { "data": <data> }
// this makes accessing that simple to use the pre-generated
// parser from graphql_ppx_re
let getData = json => json |> from_string |> Util.member("data");

let query = (~onLoad, request) => {
  let query =
    "{\"query\": \""
    ++ String.map(c => c === '\n' ? ' ' : c, request#query)
    ++ "\" }";
  print_endline(query);
  let body = Cohttp_lwt.Body.of_string(query);

  Client.post(~headers, ~body, uri)
  >>= (
    ((resp, body)) => {
      body
      |> Cohttp_lwt.Body.to_string
      >|= (body => body |> getData |> request#parse |> onLoad);
    }
  )
  |> Lwt_main.run;
};
