package com.jvm_bloggers.core.metadata;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

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
@NoArgsConstructor(force = true, access = AccessLevel.PRIVATE)
@AllArgsConstructor
public class Metadata implements Serializable {

    @Id
    @GeneratedValue(generator = "METADATA_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "METADATA_SEQ", sequenceName = "METADATA_SEQ", allocationSize = 1)
    private Long id;

    @NonNull
    @Column(name = "name", unique = true, nullable = false, length = 100)
    private String name;

    @NonNull
    @Column(name = "value", nullable = false, length = 5000)
    private String value;

}
