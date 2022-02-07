pub trait DTO<T, E> {
    fn from_dto(data: &T) -> Result<Self, E>
    where
        Self: Sized;
    fn to_dto(&self) -> Result<T, E>;
}
