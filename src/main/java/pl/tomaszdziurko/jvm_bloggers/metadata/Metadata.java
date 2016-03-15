package pl.tomaszdziurko.jvm_bloggers.metadata;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;

@Entity
@Table(name = "metadata")
@Data
@NoArgsConstructor
public class Metadata implements Serializable {

    @Id
    @GeneratedValue(generator = "SETTING_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "SETTING_SEQ", sequenceName = "SETTING_SEQ", allocationSize = 1)
    private Long id;

    @Column(name = "name", unique = true, nullable = false, length = 100)
    private String name;

    @Column(name = "value", nullable = false, length = 2500)
    private String value;

}
