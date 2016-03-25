package pl.tomaszdziurko.jvm_bloggers.metadata;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "metadata")
@Data
@NoArgsConstructor
public class Metadata implements Serializable {

    @Id
    @GeneratedValue(generator = "METADATA_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "METADATA_SEQ", sequenceName = "METADATA_SEQ", allocationSize = 1)
    private Long id;

    @Column(name = "name", unique = true, nullable = false, length = 100)
    private String name;

    @Column(name = "value", nullable = false, length = 2500)
    private String value;

    public Metadata(String name, String value) {
        this.name = name;
        this.value = value;
    }

}
