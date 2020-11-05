pub mod introspection;
pub mod statistics;

#[derive(Debug, Clone)]
pub struct Schema {
    schema: introspection::Schema,
}

pub struct Statistics {
    field_count: i32,
    type_count: i32,
}

trait NamedType {
    fn name(&self) -> String;
}

pub struct ObjectType {}
