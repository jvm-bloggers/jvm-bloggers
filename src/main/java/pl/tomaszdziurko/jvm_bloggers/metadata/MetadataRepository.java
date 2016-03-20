package pl.tomaszdziurko.jvm_bloggers.metadata;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MetadataRepository extends JpaRepository<Metadata, Long> {

    Metadata findByName(String name);

}
