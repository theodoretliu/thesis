# Semester 1 Thoughts and Work

## Goal

In my thesis, I would like to provide official and practical typing support for
the `numpy` and related linear algebra libraries. Mainly, I am interested in
preventing dimensionality errors that can occur during common matrix operations,
including dot products, matrix multiplication, and concatenation.

## Contents

In this Markdown file, I'll detail my progress over the course of the semester
on my thesis project. My work proceeded more or less sequentially as follows:

1. Research the common `numpy` operators and formally detail their behavior.
2. Reach out to the maintainers of `mypy` and understand the typing primitives
   that are currently available and are planned.
3. Identify potential shortcomings of the current primitives and develop and
   submit a proposal for new primitives and operators for `mypy`.
4. Receive and digest feedback from `mypy` maintainers. Propose final plans for
   contribution to `mypy` module.
5. Make first contribution to `mypy` codebase.
6. Research into other potential methods of typing Python, drawing inspiration
   from Liquid Haskell.
7. Plan contributions for next semester as well as further research interests.

## Outline of Common `numpy` Objects and Operators

The fundamental unit of interest in `numpy` or `np` is the `np.array` object,
which is the primary matrix/data object. The `np.array` can hold an array of
numbers, and its shape is accessible via the `.shape` attribute. To get a better
understanding of how the `.shape` attribute works, we can go over some examples:

- Suppose we have a simple array of 12 integers. This is an `np.array` with
  `.shape = (12,)`.

  ```python
  x = np.array(list(range(12)))
  print(x.shape)
  >>> (12,)
  ```

- Suppose we have a dataset of 12000 points which each have two features. The
  shape of this `np.array` is `(12000, 2)`.

- Suppose we have a dataset of 12000 images which are each 3x3 in size. The
  shape of this `np.array` is `(12000, 3, 3)`.

- Even scalars can be considered `np.array` types with `.shape` being the empty
  tuple.

So in general, the length of the `.shape` tuple indicates the number of axes
that the `np.array` has and the number in each tuple position indicates the
length of that particular axis.

---

Now, we can go over some of the most common operations on `numpy` arrays. We can
specify them by writing the type of the inputs, requirements on the relationship
between the inputs, and the shape of the output. To help make specifying the
relationships easier, we'll use some Python pseudocode at some points.

- broadcasting - this happens when we perform traditional infix operations
  between two `np.array` objects, like `*`, `/`, `**` etc, or a `np.array`
  object with something that is `array-like` (mainly just scalars that haven't
  already been converted to `np.array`s.

  - input - two arrays `A` and `B`

  - require

    ```python
    z = min(len(A.shape), len(B.shape))
    for i in range(z):
        assert (
            A[n - 1 - i] == B[m - 1 - i]
            or A[n - 1 - i] == 1
            or B[m - 1 - i] == 1
        )
    ```

    This essentially just reverses the `.shape` attributes of both arrays and
    then zips them together. It then checks for equality between all the zipped
    elements or that one of the elements is equal to 1.

  - output - single array `C` where
    `len(C.shape) == max(len(A.shape), len(B.shape))` where
    `C_i = max(A_i, B_i)`

- `np.dot` is the union type of multiple functions types. Depending on the shape
  of the `np.array`s it works on, it has multiple behaviors. `np.dot` is
  essentially the very overloaded "multiplication" operator in `numpy`. It takes
  the dot product as advertised, but when handed `2x2` matrices, it performs
  matrix multiplication as a convenience.

  - Actual dot product

    - input - two arrays `A` and `B` where `len(A.shape) == 1` and
      `len(B.shape) == 1`.
    - require
      ```python
      assert A.shape[0] == B.shape[0]
      ```
    - output - array `C` where `len(C.shape) == 0`. In other words, `C` is a
      scalar.

  - Matrix multiplication

    - input - two arrays `A` and `B` where `len(A.shape) == 2` and
      `len(B.shape) == 2`.
    - require
      ```python
      assert A.shape[1] == B.shape[0]
      ```
    - output - array `C` where `len(C.shape) == 2` and
      `C.shape[0] == A.shape[0]` and `C.shape[1] == B.shape[1]`

  - Scalar multiplication

    - input - two arrays `A` and `B` where either `len(A.shape) == 0` or
      `len(B.shape) == 0`. Without loss of generality, assume
      `len(A.shape) == 0`.
    - require - nothing
    - output - array `C` where `len(C.shape) == len(B.shape)`

  - Generalized dot product

    - input - two arrays `A` and `B` where `len(B.shape) == 1`.
    - require
      ```python
      assert A.shape[-1] == B.shape[0]
      ```
    - output - array `C` where `len(C.shape) == len(A.shape) - 1` and
      `C.shape[i] == A.shape[i] for i in range(len(C.shape))`

  - Generalized multiplication

    - input - two arrays `A` and `B` whose dimensions meet none of the criterion
      above.
    - require
      ```python
      assert A.shape[-2] == B.shape[-1]
      ```
    - output - array `C` where
      `C.shape == A.shape[:-2] + A.shape[-1:] + B.shape[:-1]`.

* `np.sum` takes the sum of the passed in `np.array`. Interestingly, it has an
  optional `axis` argument which can be used to specify the axis or axes along
  which the user would like to sum.

  - input - array `A` and optional `axis` which maybe be an `int` or `tuple`
  - require
    ```python
    assert isinstance(axis, int) or isinstance(axis, Tuple[int])
    if isinstance(axis, int):
        assert axis < len(A.shape)
    else:
        assert len(set(axis)) == len(axis) # assure no duplicates
        for ax in axis:
            assert ax < len(A.shape)
    ```
  - output - array `B` where all the provided axes are removed from `A.shape`.
    If no `axis` was provided, then `B.shape == 1`.

* `np.linalg.norm` behaves identically to `np.sum` except it computes the norm
  along a particular axis. But the shape result is identical.

* `np.zeros` and `np.ones` behave identically shapewise although the first
  creates an array of zeros and the other of ones

  - input - tuple of integers `x`
  - require - nothing
  - output - array `A` where `A.shape == x`

- `np.concatenate` concatenates two arrays together along an axis

  - input - list of `np.array`s `As`, optionally provide `axis` with default
    value of `0`
  - require

    ```python
    for A in As:
        assert A.shape == As[0].shape

    n = len(A[0].shape)
    assert axis < n

    # this loop asserts that all the arrays agree in all dimensions except the specified axis
    for i in range(n):
        if i != axis:
            assert all(A.shape[i] == As[0].shape[i] for A in As)
    ```

  - output - array `C` with `len(C.shape) == len(As[0].shape)` and
    `C.shape[i] == As[0].shape[i] for i != axis` and
    `C.shape[axis] == sum(A.shape[axis] for A in As)`.

## Original Proposal for `numpy` Typing

After reaching out to the Python community, the original proposal for `numpy`
typing was shared with me. The new type object is called a "variadic generic"
and this in combination with the existing `Literal` and `TypeVar` types are
proposed to solve the `numpy` typing problem.

### Variadic Generics

Instantiate with

```python
Ts = TypeVar('Ts', variadic=True)
```

Can be used in varidaic type constructors such as `Tuple` like in

```python
Tuple[int, Expand[Ts]]
```

or in the case of a potential `numpy` type `Shape`:

```python
Shape[Ts]
```

### Use in Typing Array Operations

Suppose we want to type an operation that drops the first axis of an array. We
define a new `Shape` variadic type as well as `IntVar` to create type variables
that stand for types. We might type this as follows:

```python
N = IntVar('N')
Ns = IntVar('Ns', variadic=True)

def drop_first(A: Shape[N, Ns]) -> Shape[Ns]:
    ...
```

For some of the functions we mentioned above, we can use the `@overload`
directive in `mypy` in conjunction with the `Literal` type constructor. For
`np.mean`, we might have

```python
@overload
def mean(array: Shape[N, Ns],
         axis: Literal[0]) -> Shape[Ns]: ...

@overload
def mean(array: Shape[N, M, Ns],
         axis: Literal[1]) -> Shape[N, Ns]: ...

@overload
def mean(array: Shape[Ns, N],
         axis: Literal[-1]) -> Shape[Ns]: ...
```

## Criticisms and Suggested Improvements of Original Proposal

Note: I shared a modified version of the following with the maintainers of
`mypy` to get their thoughts. I'll cover their response in the next section.

I think the Variadic types are very flexible, and in general, I think they will
be very useful for defining types of many `numpy` operators. However, I have
some concerns about functions like `np.mean` though. In the current proposal (as
far as I can tell), they are implemented as

```python
@overload
def mean(array: Shape[N, Ns],
         axis: Literal[0]) -> Shape[Ns]: ...

@overload
def mean(array: Shape[N, M, Ns],
         axis: Literal[1]) -> Shape[N, Ns]: ...

@overload
def mean(array: Shape[Ns, N],
         axis: Literal[-1]) -> Shape[Ns]: ...
```

which is correct, but also extremely inflexible. Even if we were to generate
hundreds of these stubs, we still would be unable to capture completely all
possible use cases of the `mean` function for `numpy` arrays with hundreds of
axes. Although such a situation is hard to imagine, it is not impossible, and it
would be preferable to cover any correct use. This also does not even cover the
possibility that `np.mean` can also accept a tuple argument as axis like

```python
@overload
def mean(array: Shape[M, N, Ns],
         axis: Tuple[Literal[0], Literal[1]]) -> Shape[Ns]: ...
```

which would lead to an even greater explosion of stubs to be generated.

Instead, I think we should aim for a more general type definition, one that can
adequately capture all use cases simultaneously. In the case of `mean`, we can
target something like

```python
@overload
def mean(array: Shape[Ms, N, Ns],
         axis: K) -> Shape[Ms, Ns]: ...
```

However, missing from this type definition is the information that `K` specifies
the particular location of `N` to remove from the resulting shape. And even if
that information were to somehow be specified in the types, we would still be at
a loss for the case where `axis` is a tuple of integers.

Something else that I've seen proposed is the idea of a special `Drop` operator,
which would result in a type that looks like the following:

```python
@overload
def mean(array: Shape[Ms],
         axis: K) -> Shape[Drop[Ms, K]]: ...

@overload
def mean(array: Shape[Ms],
         axis: Tuple[Ks]) -> Shape[Drop[Ms, Ks]]: ...
```

This is attractive because it gives us the ability to fully specify the type
signature for even the tuple case. However, it does feel very "special-cased"
and might not extend well to the signature of other methods in `numpy` or the
signature of methods that people themselves want to define. Also, as an aside,
the variadic `Ks` in the second type definition above does not capture the fact
that all the `Ks` must be distinct within the `axis` tuple.

---

As another case study, I would like to look at typing the function
`np.concatenate`, which I feel is fairly common in `numpy` use cases. Again, we
want to avoid generating stubs for specific axis arguments for `np.concatenate`,
so we can turn to the other two methods as mentioned above. As a first attempt,
we might write

```python
def concatenate(arrs: List[Shape[Ms, N, Ns]]
                axis: K) -> Shape[Ms, Sum[N], Ns]: ...
```

This captures the fact that all the lists must have the same number of axes and
those axes must align in all but one position. But this obviously has a lot of
problems. Again, we run into the issue of specifying that `K` must be the same
as the index of the `N` in the list of arrays. And also, this type declaration
seems to imply that `N` must be the same across all the arrays when in fact it
is allowed to be different between the arrays. And also, we rely on a `Sum`
construct which doesn't actually exist.

The other approach doesn't seem very promising either; it's unclear that `Drop`
will help here and neither would an `Index` construct.

---

What the `concatenate` example seems to indicate is that our type system isn't
flexible enough to capture all the operations that we might be interested in
defining within the `numpy` library. If we wanted to have full flexibility, we
might imagine a system where we can actually define functions that receive the
types of the inputs to the function as inputs to a function that simply outputs
the type as its returned type. Such a system is tested in `demo.py` where we
define **in Python** the type signatures for several functions. For example, the
`concatenate` function type might be written as

```python
def concatenate_typ(arr_typs, axis):
    if any(len(arr_typ) != len(arr_typs[0]) for arr_typ in arr_typs):
        return None

    final_type = []
    for i, *axes in enumerate(zip(*arr_typs)):
        if i == axis:
            final_type.append(sum(axes))
            continue
        if any(axis != axes[0] for axis in axes):
            return None

        final_type.append(axes[0])

    return final_type
```

This approach clearly has a lot of shortcomings. It's extremely verbose; a
nontrivial amount of code needed to be written just for this "type". It would
require the exposing of `mypy` internals so that users would be able to access
the `mypy` inferred types to make judgements about the final type. It would
require `mypy` to execute arbitrary user code, which is also not desirable.
Despite all these shortcomings, it is (so far) the only method that seems to
able to adequately capture the full type information of `concatenate`.

---

We need to find a middle ground here that gives us more expressivity without the
cost of verbosity. I'm going to suggest one possibility here, but I'm definitely
open to suggestions and/or feedback!

One possibility to solve this issue is to hybridize the two variadic methods
above and introduce a `@requires` and `@ensures` decorator to function
signatures where we can specify "contracts" about the types of the arguments.
This gives us the flexibility to enforce further constraints on types that can't
be specified by the syntax of the type. As an example with `mean`:

```python
@overload
@requires(axis == IndexOf[N])
def mean(arr: Shape[Ms, N, Ns],
         axis: K) -> Shape[Ms, Ns]: ...
```

I recognize that this is a little wonky since the `@requires` refers to `axis`
which isn't yet bound within the scope of the program. We could fix this by
making the `@requires` take a lambda:

```python
@overload
@requires(lambda _, axis: axis == IndexOf[N])
def mean(arr: Shape[Ms, N, Ns],
         axis: K) -> Shape[Ms, Ns]: ...
```

If that still doesn't seem satisfactory, we can always iterate to improve on it!
There is also the construct of `IndexOf` which doesn't yet exist within the
`mypy` system.

For the case of `mean` with `Tuple` axis argument, we might write

```python
@overload
@requires(lambda axis: all(ax1 != ax2 for i, ax1 in enumerate(axis) for ax2 in axis[i + 1:])) # uniqueness property
def mean(arr: Shape[Ms],
         axis: Tuple(Ks)) -> Shape[Drop[Ms, Ks]]: ...
```

For the concatenate operator, we'd need several operators:

```python
@requires(all(len(arr) == len(arrs[0]) for arr in arrs))
@requires(all(ax == axs[0] for i, *axs in zip(arrs) for ax in axs if i != K))
@ensures(all(out_ax == ax if i != K else out_ax == sum(axs) for i, *axs in zip(arrs) for ax in axs))
def concatenate(arrs: List[Shape],
                axis: K) -> Shape: ...
```

Again, this has its host of problems since it refers to unbound variables and
the specification of the output type is still hazy, but it still captures the
ideas behind the `@requires` and `@ensures` clauses.

## Ivan's Response to Suggestions

Ivan was kind enough to reply to my writeup from above and offer feedback.

First, he noted as an aside that that the concern about writing thousands of
stubs for `np.mean` was mostly unfounded since people rarely used more than 7 or
so axes. This is partly from a practicality standpoint and because of "the curse
of dimensionality" causing a greater number of axes to be infeasible anyways. He
also cited the fact that the current implementations of `map` and `zip` within
the Python standard library only have 7 overloads each and no one in all of
`mypy`'s usage has ever complained.

He did agree with the need for a `Drop[Ts, K]` operator and even a "double
variadic" form of `Drop[Ts, Ks]`.

In Ivan's industry experience with static type checkers and literally meeting
with the data science teams that would use `mypy` and `numpy` type-checking, it
turns out that people _don't_ want to write very complicated function signatures
most of the time. 90% of the time, they can write very simple signatures with
the primitives that have been defined.

For the very complicated functions (that other 10% of the time), `mypy` supports
a plugin system where people can write very precise type-checking for certain
types of functions. This includes functions like `concatenate` which I noted in
my writeup were very difficult to express.

As overall notes, Ivan noted two things:

1. "Practicality beats purity" - in Python and `mypy`, sometimes shortcuts are
   taken so that it is actually useful for the average person without getting
   too bogged down in theory and syntax.

2. It's better to make progress in small steps and then iterate to refine the
   product. Once a minimum working product has been completed, it can then be
   adjusted, refined, and expanded to fit more situations.

Overall, Ivan would like me to start working on a minimal implementation in
`mypy` as soon as possible, which I intend to do over winter break and through
next semester.

## First Contributions to `mypy`

Albeit small, I made a contribution to `mypy` during the course of the semester.
This was my first chance at downloading the codebase, interacting with parts of
it, running and writing tests, and getting code reviewed by the maintainers. You
can find the merged PR [here](https://github.com/python/mypy/pull/7971).

## Plan for the Winter and Upcoming Semester

Ultimately, I would like to make a major contribution to `mypy` by implementing
these `numpy` typings, even in their most basic form, so that I can lay the
groundwork for more serious typing work for Python in the future.

To ramp up so far, I've been looking at various bug reports or feature requests
and taking care of them incrementally. Ivan has already laid out a path to the
`numpy` typing, which is roughly:

- General support for "variadicity" and ArgSpecs
- Support for array-arity declaration/checks
- Support for fixed shape types (integer generics)
- Support for algebra of dimensions

I will work more closely with him and communicate more frequently to get
guidance on the specific implementation details, and I hope to accomplish at
least everything he has laid out by the end of the next semester.

In addition to contributing to the main `mypy`, I am also interested in
embedding these matrix operations into other, more fully featured languages to
investigate the practicality of the typing primitives. I discuss this further in
the next section.

## Other Typing Systems and Their Relevance

Liquid Haskell is a refinement type system built on top of Haskell. This system
allows for logical predicates to be embedded in the types of functions and
arguments, which almost exactly corresponds to the `@requires` and `@ensures`
clauses from earlier.

As an exploration into the feasibility, usability, and usefullness of full
refinement types in matrix operations, I propose implementing a fully featured
matrix library in Liquid Haskell or an equivalently strong type-system to see if
the tradeoffs are worth bringing over to Python.
