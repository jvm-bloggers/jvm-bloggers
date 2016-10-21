package com.jvm_bloggers.kafka;

import akka.Done;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.japi.Pair;
import akka.kafka.ProducerSettings;
import akka.kafka.javadsl.Producer;
import akka.stream.ActorMaterializer;
import akka.stream.OverflowStrategy;
import akka.stream.javadsl.Keep;
import akka.stream.javadsl.Source;
import com.jvm_bloggers.kafka.exception.UnableToConnectToKafkaException;
import com.jvm_bloggers.kafka.serialization.MessageSerializer;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.serialization.ByteArraySerializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletionStage;

@Service
@Slf4j
class MessagesPublisher {

    private final ActorRef publishingActor;
    private final MessageSerializer messageSerializer;

    @Autowired
    public MessagesPublisher(ActorSystem actorSystem,
                             ActorMaterializer actorMaterializer,
                             KafkaConfiguration configuration,
                             MessageSerializer messageSerializer) {
        ProducerSettings<byte[], String> producerSettings = ProducerSettings
                .create(actorSystem, new ByteArraySerializer(), new StringSerializer())
                .withBootstrapServers(configuration.getAddress());
        Pair<ActorRef, CompletionStage<Done>> materialization = Source
                .<ProducerRecord<byte[], String>>actorRef(configuration.getStreamBufferSize(),
                        OverflowStrategy.dropNew())
                .toMat(Producer.plainSink(producerSettings), Keep.both())
                .run(actorMaterializer);
        CompletionStage<Done> connectionResult = materialization.second();
        handleConnectionFailure(configuration, connectionResult);
        publishingActor = materialization.first();
        this.messageSerializer = messageSerializer;
    }

    public <T> void publish(T message, String topic) {
        String serializedMessage = messageSerializer.serialize(message);
        publish(serializedMessage, topic);
    }

    private void publish(String jsonMessage, String topic) {
        ProducerRecord<byte[], String> message = new ProducerRecord<>(topic, jsonMessage);
        publishingActor.tell(message, ActorRef.noSender());
    }

    private void handleConnectionFailure(KafkaConfiguration configuration,
                                         CompletionStage<Done> result) {
        result.exceptionally(cause -> {
            throw new UnableToConnectToKafkaException(cause, configuration);
        });
    }
}
