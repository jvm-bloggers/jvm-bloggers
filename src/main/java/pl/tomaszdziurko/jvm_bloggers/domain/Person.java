package pl.tomaszdziurko.jvm_bloggers.domain;


import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "person")
public class Person {

    @Id
    @GeneratedValue(generator = "PERSON_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "PERSON_SEQ", sequenceName = "PERSON_SEQ", allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @Column(name = "NAME", nullable = false, length = 250)
    private String name;

    @Column(name = "RSS", unique = true, nullable = false, length = 250)
    private String rss;

    @Column(name = "TWITTER", nullable = true, length = 100)
    private String twitter;

    public Person(String name, String rss, String twitter) {
        this.name = name;
        this.rss = rss;
        this.twitter = twitter;
    }

    public Person() {
    }
}
