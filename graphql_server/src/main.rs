extern crate actix_cors;
extern crate log;
use actix_cors::Cors;
use actix_web::{middleware, web, App, Error, HttpResponse, HttpServer};
use juniper::http::graphiql::graphiql_source;
use juniper::http::GraphQLRequest;
use std::io;
use std::sync::Arc;

use juniper::FieldResult;

struct DatabasePool;
impl DatabasePool {
    fn get_connection(&self) -> FieldResult<DatabasePool> {
        Ok(DatabasePool)
    }
    fn find_human(&self, _id: &str) -> FieldResult<Human> {
        Err("")?
    }
    fn insert_human(&self, _human: &NewHuman) -> FieldResult<Human> {
        Err("")?
    }
}

#[derive(juniper::GraphQLEnum)]
enum Episode {
    NewHope,
    Empire,
    Jedi,
}

#[derive(juniper::GraphQLObject)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct Human {
    id: String,
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

// There is also a custom derive for mapping GraphQL input objects.

#[derive(juniper::GraphQLInputObject)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct NewHuman {
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

// Now, we create our root Query and Mutation types with resolvers by using the
// object macro.
// Objects can have contexts that allow accessing shared state like a database
// pool.

struct Context {
    // Use your real database pool here.
    pool: DatabasePool,
}

// To make our context usable by Juniper, we have to implement a marker trait.
impl juniper::Context for Context {}

struct Query;

#[juniper::object(
    // Here we specify the context type for the object.
    // We need to do this in every type that
    // needs access to the context.
    Context = Context,
)]
impl Query {
    fn apiVersion() -> &str {
        "1.0"
    }

    // Arguments to resolvers can either be simple types or input objects.
    // To gain access to the context, we specify a argument
    // that is a reference to the Context type.
    // Juniper automatically injects the correct context here.
    fn human(context: &Context, id: String) -> FieldResult<Human> {
        // Get a db connection.
        let connection = context.pool.get_connection()?;
        // Execute a db query.
        // Note the use of `?` to propagate errors.
        let human = connection.find_human(&id)?;
        // Return the result.
        Ok(human)
    }
}

// Now, we do the same for our Mutation type.
struct Mutation;

#[juniper::object(
    Context = Context,
)]
impl Mutation {
    fn createHuman(context: &Context, new_human: NewHuman) -> FieldResult<Human> {
        let db = executor.context().pool.get_connection()?;
        let human: Human = db.insert_human(&new_human)?;
        Ok(human)
    }
}

// A root schema consists of a query and a mutation.
// Request queries can be executed against a RootNode.
type Schema = juniper::RootNode<'static, Query, Mutation>;

async fn graphiql() -> HttpResponse {
    let html = graphiql_source("http://127.0.0.1:8080/graphql");
    HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(html)
}

async fn graphql(
    st: web::Data<Arc<Schema>>,
    data: web::Json<GraphQLRequest>,
) -> Result<HttpResponse, Error> {
    let user = web::block(move || {
        let res = data.execute(
            &st,
            &Context {
                pool: DatabasePool {},
            },
        );
        Ok::<_, serde_json::error::Error>(serde_json::to_string(&res)?)
    })
    .await?;
    Ok(HttpResponse::Ok()
        .content_type("application/json")
        .body(user))
}

#[actix_web::main]
async fn main() -> io::Result<()> {
    std::env::set_var("RUST_LOG", "actix_web=info");
    env_logger::init();

    // Create Juniper schema
    let schema = std::sync::Arc::new(Schema::new(Query, Mutation));
    println!("Access service at http://localhost:8080/graphiql");

    // Start http server
    HttpServer::new(move || {
        let cors = Cors::permissive().max_age(3600);

        App::new()
            .data(schema.clone())
            .wrap(middleware::Logger::default())
            .wrap(cors)
            .service(web::resource("/graphql").route(web::post().to(graphql)))
            .service(web::resource("/graphiql").route(web::get().to(graphiql)))
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
