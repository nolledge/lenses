/**
 * A lens takes an object and a value and provides setter anc getter functions
 * @tparam O object type setter and getter operates on
 * @tparam V value type getting set
 */
sealed trait Lens[O, V] {
  val get: O => V
  val set: (O, V) => O
}

object Lens{
  def compose[Outer, Inner, Value](outer: Lens[Outer, Inner], inner: Lens[Inner, Value]) = new Lens[Outer, Value] {
    override val get: Outer => Value = outer.get andThen inner.get
    override val set: (Outer, Value) => Outer = (o, v) => outer.set(o, inner.set(outer.get(o), v))
  }
}
/**
 * Example data with nested structure
 */
case class Person(name: String, age: Int, address: Address)
case class Address(street: String, houseNumber: Int)

/**
 * A Lens to update an address on a person
 */
val addressLens: Lens[Person, Address] = new Lens[Person, Address] {
  override val get: Person => Address = p => p.address
  override val set: (Person, Address) => Person = (p, a) => p.copy(address = a)
}
/**
 * A Lens to update a houseNum on an address
 */
private val houseNumLens: Lens[Address, Int] = new Lens[Address, Int] {
  override val get: Address => Int = a => a.houseNumber
  override val set: (Address, Int) => Address = (a, n) => a.copy(houseNumber = n)
}

/**
 * The types align! Lenses compose!
 */
val addressHouseNumLens: Lens[Person, Int] = Lens.compose(addressLens, houseNumLens)

val testPerson = Person("Christof", 30, Address("fake street", 5))

// update by copy
val testPersonUpdateCopy = testPerson.copy(address = testPerson.address.copy(houseNumber = 2))

// update by lens
val testPersonUpdateLens = addressHouseNumLens.set(testPerson, 2)

