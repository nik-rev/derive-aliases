Copy = Copy, Clone;
Eq = PartialEq, Eq;
Ord = PartialOrd, Ord, ..Eq;
Together = PartialOrd, std::hash::Hash;
