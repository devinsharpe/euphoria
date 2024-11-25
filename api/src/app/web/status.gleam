import app/web.{type Context}
import gleam/http.{Get}
import gleam/json
import wisp.{type Request, type Response}

pub fn status(req: Request, ctx: Context) -> Response {
  case req.method {
    Get ->
      wisp.json_response(
        json.to_string_tree(
          json.object([#("status", json.string(ctx.app_status))]),
        ),
        200,
      )
    _ -> wisp.method_not_allowed([Get])
  }
}
