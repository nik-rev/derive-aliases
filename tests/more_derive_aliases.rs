#[cfg(feature = "arbitrary")]
Copy =
    #[cfg(feature = "arbitrary")]
    Copy,
    #[cfg(feature = "arbitrary")]
    Clone;

#[cfg(feature = "arbitrary")]
Eq = PartialEq, Eq;
