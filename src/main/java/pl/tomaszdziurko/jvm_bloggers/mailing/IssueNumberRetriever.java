package pl.tomaszdziurko.jvm_bloggers.mailing;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;

@Repository
public class IssueNumberRetriever {

    private JdbcTemplate jdbcTemplate;

    //Required by Wicket when injected using @SpringBean
    public IssueNumberRetriever() {
    }

    @Autowired
    public IssueNumberRetriever(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
    }

    public long getNextIssueNumber() {
        return this.jdbcTemplate.queryForObject(
            "SELECT nextval('MAILING_ISSUE_NUMBER_SEQ')", Long.class
        );
    }

    public long getCurrentIssueNumber() {
        return this.jdbcTemplate.queryForObject(
            "SELECT last_value FROM MAILING_ISSUE_NUMBER_SEQ", Long.class
        );
    }
}
