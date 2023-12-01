package hello

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloWorldSpec extends AnyFlatSpec with Matchers {
  it should "say hello" in {
    HelloWorld.sayHello() shouldBe "World!"
  }
}
