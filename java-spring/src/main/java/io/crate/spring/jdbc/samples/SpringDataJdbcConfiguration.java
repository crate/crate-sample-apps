package io.crate.spring.jdbc.samples;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.relational.core.dialect.Dialect;
import org.springframework.data.relational.core.dialect.PostgresDialect;
import org.springframework.jdbc.core.ConnectionCallback;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;

import java.sql.Connection;
import java.sql.SQLException;

@Configuration
public class SpringDataJdbcConfiguration extends AbstractJdbcConfiguration {
    /**
     * Provide a custom JDBC configuration to set the dialect for the `crate` JDBC driver.
     * For now, Spring Data JDBC's `PostgresDialect` is used as a reasonable default.
     * -- https://stackoverflow.com/a/62545740
     *
     * @param operations the {@link NamedParameterJdbcOperations} allowing access to a {@link java.sql.Connection}.
     * @return
     */
    @Override
    public Dialect jdbcDialect(NamedParameterJdbcOperations operations) {
        return operations.getJdbcOperations().execute((ConnectionCallback<Dialect>)
                connection -> isCrateDB(connection) ? PostgresDialect.INSTANCE : super.jdbcDialect(operations));
    }

    private boolean isCrateDB(Connection connection) throws SQLException {
        return connection.getMetaData().getDatabaseProductName().toLowerCase().contains("crate");
    }
}
