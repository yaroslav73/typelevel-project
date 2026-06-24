package example.project.jobsboard.core

import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Args
import org.scalatest.Status
import example.project.jobsboard.fixtures.UserFixture
import cats.effect.IO
import example.project.jobsboard.domain.User
import cats.instances.uuid

class UsersSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with Database with UserFixture {

  val initStript: String = "sql/users.sql"

  "Users 'algebra'" - {
    "should not return a user if the given UUID does not exist" in {
      transactor.use { xa =>
        val users  = Users.of[IO](xa)
        val result = users.find(NotFoundUserId)

        result.asserting(_ shouldBe None)
      }
    }

    "should not return a user if the given emaiil does not exist" in {
      transactor.use { xa =>
        val users  = Users.of[IO](xa)
        val result = users.find(NotFoundUserEmail)

        result.asserting(_ shouldBe None)
      }
    }

    "should return a user by given UUID" in {
      transactor.use { xa =>
        val users  = Users.of[IO](xa)
        val result = users.find(john.id)

        result.asserting(_ shouldBe Some(john))
      }
    }

    "should return a user by given email" in {
      transactor.use { xa =>
        val users  = Users.of[IO](xa)
        val result = users.find(john.email)

        result.asserting(_ shouldBe Some(john))
      }
    }

    "should create a new user" in {
      transactor.use { xa =>
        val newAdmin = User.New.admin("test_admin@email.com", "hashedpassword", None, None, None)

        val users = Users.of[IO](xa)

        val result = for {
          uuid    <- users.create(newAdmin)
          userOpt <- users.find(uuid)
        } yield userOpt

        result.asserting { userOpt =>
          userOpt should not be None
          userOpt.map(_.email) shouldBe Some(newAdmin.email)
        }
      }
    }

    "should update user by given UUID" in {
      transactor.use { xa =>
        val users       = Users.of[IO](xa)
        val updatedAnna = anna.copy(firstName = Some("Anna II"), lastName = Some("Maria"))
        val result      = users.update(updatedAnna)

        result.asserting(_ shouldBe Some(updatedAnna))
      }
    }

    "should not update user by given not existen UUID" in {
      transactor.use { xa =>
        val users  = Users.of[IO](xa)
        val result = users.update(anna.copy(id = NotFoundUserId))

        result.asserting(_ shouldBe None)
      }
    }

    "should delete job by given UUID" in {
      transactor.use { xa =>
        val users = Users.of[IO](xa)
        val result = for {
          delete  <- users.delete(anna.id)
          userOpt <- users.find(anna.id)
        } yield (delete, userOpt)

        result.asserting(_ shouldBe (true, None))
      }
    }
  }
}
