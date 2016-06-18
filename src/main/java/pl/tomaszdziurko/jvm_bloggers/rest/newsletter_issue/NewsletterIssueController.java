package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;


import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;

import static pl.tomaszdziurko.jvm_bloggers.rest.ContentTypes.JVM_BLOGGERS_V1;

@RestController
@RequestMapping(path = "/issues")
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class NewsletterIssueController {

    private final GetLatestNewsletterIssueService latestNewsletterIssueService;

    @RequestMapping(
        method = RequestMethod.GET,
        path = "/latest",
        consumes = JVM_BLOGGERS_V1,
        produces = JVM_BLOGGERS_V1
    )
    public ResponseEntity getLatestIssue() {
        Optional<NewsletterIssueDto> latestIssue = latestNewsletterIssueService.getLatestIssue();

        if (latestIssue.isPresent()) {
            return ResponseEntity.ok(latestIssue.get());
        } else {
            return ResponseEntity.notFound().build();
        }
    }
}
