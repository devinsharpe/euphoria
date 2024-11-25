import app/web.{type Context}
import app/web/status
import wisp.{type Request, type Response}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- web.middleware(req)

  case wisp.path_segments(req) {
    ["status"] -> status.status(req, ctx)
    _ -> wisp.not_found()
  }
}
