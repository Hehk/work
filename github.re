open Lwt;
open Cohttp;
open Cohttp_lwt_unix;


let query = body => {
  let headers = Header.init() |> Header.add(_, "Authorization", "Bearer " ++ Env.githubAccessToken);
  let body = Cohttp_lwt.Body.of_string(body);
  let uri = Uri.of_string("https://api.github.com/graphql");

  Client.post(~headers, ~body, uri)
  >>= (
    ((resp, body)) => {
      let code = resp |> Response.status |> Code.code_of_status;
      Printf.printf("Response code: %d\n", code);
      Printf.printf(
        "Headers: %s\n",
        resp |> Response.headers |> Header.to_string,
      );
      body
      |> Cohttp_lwt.Body.to_string
      >|= (
        body => {
          print_endline("Body:" ++ body);
          body;
        }
      );
    }
  );
};
