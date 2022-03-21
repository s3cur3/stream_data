defmodule Sandbox.Stream do
  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    {:cont, acc}
  end

  defmacrop next(fun, entry, acc) do
    quote(do: unquote(fun).(unquote(entry), unquote(acc)))
  end

  defmacrop acc(head, state, tail) do
    quote(do: [unquote(head), unquote(state) | unquote(tail)])
  end

  defmacrop next_with_acc(fun, entry, head, state, tail) do
    quote do
      {reason, [head | tail]} = unquote(fun).(unquote(entry), [unquote(head) | unquote(tail)])
      {reason, [head, unquote(state) | tail]}
    end
  end

  # REFRENCES:
  # https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/stream.ex
  # https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/enum.ex#L2626
  # https://github.com/elixir-lang/elixir/pull/11349

  @doc """
  Slides a single or multiple elements given by `range_or_single_index` from `enumerable`
  to `insertion_index`.
  The semantics of the range to be moved match the semantics of `Enum.slice/2`.
  Specifically, that means:
   * Indices are normalized, meaning that negative indexes will be counted from the end
      (for example, -1 means the last element of the enumerable). This will result in *two*
      traversals of your enumerable on types like lists that don't provide a constant-time count.
    * If the normalized index range's `last` is out of bounds, the range is truncated to the last element.
    * If the normalized index range's `first` is out of bounds, the selected range for sliding
      will be empty, so you'll get back your input list.
    * Decreasing ranges (such as `5..0//1`) also select an empty range to be moved,
      so you'll get back your input list.
    * Ranges with any step but 1 will raise an error.
  ## Examples
      # Slide a single element
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], 5, 1)
      [:a, :f, :b, :c, :d, :e, :g]
      # Slide a range of elements backward
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], 3..5, 1)
      [:a, :d, :e, :f, :b, :c, :g]
      # Slide a range of elements forward
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], 1..3, 5)
      [:a, :e, :f, :b, :c, :d, :g]
      # Slide with negative indices (counting from the end)
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], 3..-1//1, 2)
      [:a, :b, :d, :e, :f, :g, :c]
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], -4..-2, 1)
      [:a, :d, :e, :f, :b, :c, :g]
      # Insert at negative indices (counting from the end)
      iex> Stream.slide([:a, :b, :c, :d, :e, :f, :g], 3, -1)
      [:a, :b, :c, :e, :f, :g, :d]
  """
  @doc since: "1.14.0"
  @spec slide(Enumerable.t(), Range.t() | non_neg_integer, non_neg_integer) :: Enumerable.t()
  def slide(enum, range_or_single_index, insertion_index)

  def slide(enum, single_index, insertion_index) when is_integer(single_index) do
    slide(enum, single_index..single_index, insertion_index)
  end

  # This matches the behavior of Enum.slice/2
  def slide(_, _.._//step = index_range, _insertion_index) when step != 1 do
    raise ArgumentError,
          "Stream.slide/3 does not accept ranges with custom steps, got: #{inspect(index_range)}"
  end

  def slide(_, first..last = index_range, _) when first < 0 or last < 0 do
    raise ArgumentError,
          "Stream.slide/3 does not accept negative indices, got: #{inspect(index_range)}"
  end

  def slide(_, _, insertion_index) when insertion_index < 0 do
    raise ArgumentError,
          "Stream.slide/3 does not accept a negative insertion index, got: #{inspect(insertion_index)}"
  end

  def slide(_, first..last = index_range, insertion_index)
      when insertion_index <= last and insertion_index > first do
    raise ArgumentError,
          "Stream.slide/3 insertion index must be outside the range to move, got: #{inspect(index_range)} moved to #{inspect(insertion_index)}"
  end

  def slide(enum, first..last, insertion_index) when insertion_index <= first do
    slide_stream(enum, insertion_index, first, last)
  end

  def slide(enum, first..last, insertion_index) do
    slide_stream(enum, first, last + 1, insertion_index)
  end

  # Takes the range from middle..last and moves it to be in front of index start
  defp slide_stream(enum, first, middle, last) do
    emitted_head = Stream.take(enum, first - 1)
    start_to_middle = Stream.take(emitted_head, middle - first)
    middle_to_last = lazy(%{start_to_middle | done: nil}, last - middle, &R.take/1, true)
    last_to_end = %{middle_to_last | done: nil}

    Stream.concat([emitted_head, middle_to_last, middle_to_last, last_to_end])
  end

  defp lazy(%Stream{done: nil, funs: funs} = lazy, fun), do: %{lazy | funs: [fun | funs]}
  defp lazy(enum, fun), do: %Stream{enum: enum, funs: [fun]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun),
    do: %{lazy | funs: [fun | funs], accs: [acc | accs]}

  defp lazy(enum, acc, fun), do: %Stream{enum: enum, funs: [fun], accs: [acc]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun, done),
    do: %{lazy | funs: [fun | funs], accs: [acc | accs], done: done}

  defp lazy(enum, acc, fun, done), do: %Stream{enum: enum, funs: [fun], accs: [acc], done: done}
end
