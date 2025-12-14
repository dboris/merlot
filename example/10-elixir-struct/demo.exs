# Demo: Using Merlot structs from Elixir
#
# This script demonstrates calling Merlot code that defines
# Elixir-compatible structs using [@@@elixir_struct].
#
# Run with: elixir demo.exs (after compiling person.ml)

# Load the Merlot-compiled module
Code.append_path(".")
:code.load_abs(~c"./Elixir.Person")

IO.puts("=== Merlot-Elixir Struct Interop Demo ===\n")

# Create a person using the Merlot constructor
# (Note: %Person{} syntax requires compile-time module availability,
#  so we use the constructor functions for dynamically loaded modules)
IO.puts("1. Creating a person with Person.new_/2:")
alice = Person.new_("Alice", 25)
IO.inspect(alice, label: "   Alice")

# Verify it's a proper Elixir struct
IO.puts("\n2. Checking struct type:")
IO.puts("   Is map? #{is_map(alice)}")
IO.puts("   Has __struct__ key? #{Map.has_key?(alice, :__struct__)}")
IO.puts("   Struct module: #{alice.__struct__}")

# Access fields via map syntax
IO.puts("\n3. Accessing struct fields:")
IO.puts("   Name: #{alice.name}")
IO.puts("   Age: #{alice.age}")

# Call Merlot functions on the struct
IO.puts("\n4. Calling Merlot functions:")
IO.puts("   Person.name(alice) = #{Person.name(alice)}")
IO.puts("   Person.age(alice) = #{Person.age(alice)}")
IO.puts("   Person.is_adult(alice) = #{Person.is_adult(alice)}")

# Use the birthday function (returns updated struct)
IO.puts("\n5. Using Person.birthday/1:")
older_alice = Person.birthday(alice)
IO.inspect(older_alice, label: "   After birthday")

# Create another person
IO.puts("\n6. Creating Bob with Person.new_/2:")
bob = Person.new_("Bob", 17)
IO.inspect(bob, label: "   Bob")
IO.puts("   Is Bob an adult? #{Person.is_adult(bob)}")

# Pattern matching on the map works!
IO.puts("\n7. Pattern matching on Merlot structs:")
case alice do
  %{__struct__: Person, name: name, age: age} when age >= 21 ->
    IO.puts("   #{name} can drink in the US")
  %{__struct__: Person, name: name} ->
    IO.puts("   #{name} is too young to drink in the US")
end

# Map update syntax works
IO.puts("\n8. Elixir map update syntax:")
renamed = %{alice | name: "Alicia"}
IO.inspect(renamed, label: "   Renamed")

# Using __struct__/0 to get default struct
IO.puts("\n9. Using __struct__/0 for defaults:")
default = Person.__struct__()
IO.inspect(default, label: "   Default person")

# Using __struct__/1 to create with overrides
IO.puts("\n10. Using __struct__/1 with keyword list:")
custom = Person.__struct__(name: "Charlie", age: 30)
IO.inspect(custom, label: "   Custom person")

IO.puts("\n=== Demo Complete ===")
