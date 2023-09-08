# Null

> I call it my billion-dollar mistake. It was the invention of the null
> reference in 1965. —&nbsp;Tony Hoare

One of the intended use cases of RCL is to be an abstraction layer for formats
with no or poor abstraction abilities, such as json and yaml. Many tools take
input in such format, and it is not up to RCL to make demands about the schema
of that data. RCL _has_ to provide a way to emit `null`.

Fortunately, the problems commonly attributed to null are not problems with
having a null value per se, they are problems with implicit nullability, which
RCL does not have. `null` is the sole value of type `Null`, which is distinct
from and does not unify with other types. In this sense, null behaves like the
unit type in languages such as Haskell and Rust (written `()` there).
