import app/router
import app/web
import gleam/erlang/process
import mist
import wisp
import wisp/wisp_mist

pub const data_directory = "tmp/data"

pub fn main() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  // A context is constructed to hold the database connection.
  let context = web.Context("OK")

  // The handle_request function is partially applied with the context to make
  // the request handler function that only takes a request.
  let handler = router.handle_request(_, context)

  let assert Ok(_) =
    handler
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}
