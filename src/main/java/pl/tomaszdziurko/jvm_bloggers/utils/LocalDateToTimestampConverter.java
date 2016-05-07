package pl.tomaszdziurko.jvm_bloggers.utils;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;

import javax.persistence.AttributeConverter;
import javax.persistence.Converter;

@Converter(autoApply = true)
public class LocalDateToTimestampConverter
    implements AttributeConverter<LocalDate, Timestamp> {

    @Override
    public Timestamp convertToDatabaseColumn(LocalDate attribute) {
        LocalDateTime localDateTime = attribute.atTime(12, 0);
        return Timestamp.valueOf(localDateTime);
    }

    @Override
    public LocalDate convertToEntityAttribute(Timestamp dbData) {
        LocalDateTime dateTime = dbData.toLocalDateTime();
        return dateTime.toLocalDate();
    }
}
