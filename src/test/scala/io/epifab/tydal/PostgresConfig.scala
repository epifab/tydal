package io.epifab.tydal

import java.sql.{Connection, DriverManager}

object PostgresConnection {
  def apply(config: PostgresConfig): Connection = {
    DriverManager.getConnection(config.toUrl)
  }
}

case class PostgresConfig(dbUser: String, dbPass: String, host: String, port: Int, dbName: String, ssl: Boolean) {
  def toUrl: String =
    s"jdbc:postgresql://%s:%s/%s?user=%s&password=%s&sslmode=%s".format(
      host,
      port,
      dbName,
      dbUser,
      dbPass,
      if (ssl) "require" else "disable"
    )
}

object PostgresConfig {
  def apply(url: String, ssl: Boolean): PostgresConfig = {
    val uri = new java.net.URI(url)
    uri.getUserInfo.split(':').toList match {
      case dbUser :: dbPass :: Nil =>
        PostgresConfig(dbUser, dbPass, uri.getHost, uri.getPort, uri.getPath.tail, ssl)
      case _ => throw new IllegalArgumentException("Username and password missing")
    }
  }

  def fromEnv(): PostgresConfig = PostgresConfig(
    sys.env.getOrElse("DATABASE_URL", "postgres://root:p4ssw0rd@localhost:5432/tydal"),
    sys.env.get("POSTGRES_SSL").exists(_.toBoolean)
  )
}
