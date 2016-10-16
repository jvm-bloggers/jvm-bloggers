package com.jvm_bloggers.kafka

import akka.actor.ActorSystem
import akka.kafka.ConsumerMessage
import akka.kafka.ConsumerSettings
import akka.kafka.Subscriptions
import akka.kafka.javadsl.Consumer
import akka.stream.ActorMaterializer
import akka.stream.javadsl.Keep
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.javadsl.TestSink
import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.kafka.message.NewIssuePublishedMessage
import net.manub.embeddedkafka.EmbeddedKafka$
import net.manub.embeddedkafka.EmbeddedKafkaConfig
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.ByteArrayDeserializer
import org.apache.kafka.common.serialization.StringDeserializer
import org.springframework.beans.factory.annotation.Autowired
import scala.Predef$
import scala.Tuple2
import scala.collection.JavaConverters$
import scala.collection.immutable.Map
import scala.concurrent.duration.FiniteDuration
import spock.lang.Subject

class MessagesPublisherSpecification extends SpringContextAwareSpecification {

	@Autowired
	@Subject
	MessagesPublisher producer
	@Autowired
	ActorSystem actorSystem
	@Autowired
	ActorMaterializer actorMaterializer

	def setup() {
		def kafkaSettings = javaToscalaMap(Collections.<String,String>emptyMap())
		EmbeddedKafka$.MODULE$.start(new EmbeddedKafkaConfig(9092,2181,kafkaSettings))
	}

	def cleanup() {
		EmbeddedKafka$.MODULE$.stop()
	}

    def "published message should be consumed"() {
        given:
            NewIssuePublishedMessage publishedMessage = new NewIssuePublishedMessage(15L, "http://jvm-bloggers.com/issue/15");
            String expectedConsumedMessage = """{"issueNumber":15,"url":"http://jvm-bloggers.com/issue/15"}"""
            ConsumerSettings<byte[], String> consumerSettings = ConsumerSettings
                    .create(actorSystem, new ByteArrayDeserializer(), new StringDeserializer())
                    .withBootstrapServers("127.0.0.1:9092")
                    .withGroupId("kafkaTestGroup")
                    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
            TestSubscriber.Probe<ConsumerMessage.CommittableMessage<byte[], String>> testProbe = Consumer.
                    committableSource(consumerSettings, Subscriptions.topics("com.jvm_bloggers.issue.published"))
                    .toMat(TestSink.probe(actorSystem), Keep.right())
                    .run(actorMaterializer);
        when:
            producer.publish(publishedMessage, "com.jvm_bloggers.issue.published")
            def actualConsumedMessage = testProbe.requestNext(FiniteDuration.create(20, "seconds")).record().value()
        then:
            actualConsumedMessage == expectedConsumedMessage
    }


	private Map<String, String> javaToscalaMap(java.util.Map<String, String> map) {
		JavaConverters$.MODULE$.mapAsScalaMapConverter(map).asScala().toMap(
				Predef$.MODULE$.<Tuple2<String, String>> conforms())
	}
}
