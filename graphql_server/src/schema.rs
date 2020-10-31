use juniper::FieldResult;
use std::collections::HashMap;
use uuid::Uuid;

pub struct Database {
    humans: HashMap<String, Human>,
}

impl Database {
    fn new() -> Self {
        let mut humans = HashMap::new();

        humans.insert(
            "1000".to_owned(),
            Human::new(
                "1000",
                "Luke Skywalker",
                &[Episode::NewHope, Episode::Empire, Episode::Jedi],
                "Tatooine",
            ),
        );

        Self { humans: humans }
    }

    fn find_human(&self, _id: &str) -> FieldResult<Human> {
        Err("")?
    }

    fn insert_human(&mut self, _human: &NewHuman) -> FieldResult<Human> {
        let id = Uuid::new_v4().to_string();
        let cloned = _human.clone();
        let human = Human {
            id: id.clone(),
            name: cloned.name,
            appears_in: cloned.appears_in,
            home_planet: cloned.home_planet,
        };

        self.humans.insert(id, human.clone());

        Ok(human)
    }
}

// Now, we create our root Query and Mutation types with resolvers by using the
// object macro.
// Objects can have contexts that allow accessing shared state like a database
// pool.

pub struct Context {
    // Use your real database pool here.
    pub db: Database,
}

impl Context {
    pub fn new() -> Context {
        Context {
            db: Database::new(),
        }
    }
}

// To make our context usable by Juniper, we have to implement a marker trait.
impl juniper::Context for Context {}

#[derive(juniper::GraphQLEnum, Clone)]
enum Episode {
    NewHope,
    Empire,
    Jedi,
}

#[derive(juniper::GraphQLObject, Clone)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct Human {
    id: String,
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

impl Human {
    fn new(id: &str, name: &str, appears_in: &[Episode], home_planet: &str) -> Self {
        Self {
            id: id.to_owned(),
            name: name.to_owned(),
            appears_in: appears_in.to_vec(),
            home_planet: home_planet.to_owned(),
        }
    }
}

// There is also a custom derive for mapping GraphQL input objects.

#[derive(juniper::GraphQLInputObject, Clone)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct NewHuman {
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

pub struct Query;

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
        let human = context.db.find_human(&id)?;
        // Return the result.
        Ok(human)
    }

    fn humans(context: &Context) -> FieldResult<Vec<Human>> {
        let all = context.db.humans.values().cloned().collect::<Vec<Human>>();
        Ok(all)
    }
}

// Now, we do the same for our Mutation type.
pub struct Mutation;

#[juniper::object(
    Context = Context,
)]
impl Mutation {
    fn createHuman(context: &Context, new_human: NewHuman) -> FieldResult<Human> {
        Err("")?
    }
}

// A root schema consists of a query and a mutation.
// Request queries can be executed against a RootNode.
pub type Schema = juniper::RootNode<'static, Query, Mutation>;

pub fn build() -> Schema {
    Schema::new(Query, Mutation)
}
