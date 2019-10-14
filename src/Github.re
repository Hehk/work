open Lwt;
open Cohttp;
open Cohttp_lwt_unix;

let headers = Header.init() |> Header.add(_, "Authorization", "Bearer " ++ Env.githubAccessToken);
let uri = Uri.of_string("https://api.github.com/graphql");


let query = (~onLoad, ~onError, request) => {
  let body = Cohttp_lwt.Body.of_string(request#query);

  Client.post(~headers, ~body, uri)
   >>=(
    ((resp, body)) => {
     let code = resp |> Response.status |> Code.code_of_status;
     if (code === 200) {
      body
       |> Cohttp_lwt.Body.to_string
       >|= (body => body |> request#parse |> onLoad)
     } else {
      onError()
     }
    }
   )
}
