#[allow(dead_code, unused_imports)]

#[macro_use]
extern crate rocket;

use re_rocket::*;
use rocket::serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
struct Request {
    request: String,
}

#[derive(Deserialize, Serialize)]
struct Response {
    response: String,
}

#[derive(Deserialize, Serialize)]
pub struct JsonError {
    pub error: String,
}

#[re_rocket]
#[get("/get")]
async fn create(request: Json<Request>) -> Result<Json<Response>, Json<JsonError>> {
    Ok(Json(Response {
        response: String::from("Some Response"),
    }))
}

#[re_rocket]
#[post("/post", data = "<request>")]
async fn create(request: Json<Request>) -> Result<Json<Response>, Json<JsonError>> {
    Ok(Json(Response {
        response: String::from("Some Response"),
    }))
}

#[test]
fn test_macro() {
    // due to macro we have struct H in scope
    info!("Hello from Test");

    assert!(1 == 1);
}
