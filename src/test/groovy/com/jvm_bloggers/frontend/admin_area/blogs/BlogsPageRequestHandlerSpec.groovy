package com.jvm_bloggers.frontend.admin_area.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder
import org.springframework.data.domain.PageImpl
import org.springframework.data.domain.PageRequest
import org.springframework.data.domain.Sort

class BlogsPageRequestHandlerSpec extends MockSpringContextAwareSpecification {

    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15)
    BlogRepository blogRepository = Mock(BlogRepository.class)

    @Override
    protected void setupContext() {
        addBean(paginationConfiguration)
        addBean(blogRepository)
    }

    def "Should ask for sorted by author asc"() {
        given:
        BlogsPageRequestHandler handler = new BlogsPageRequestHandler()

        when:
        handler.iterator(0, 10)

        then:
        1 * blogRepository.findAll((PageRequest) _) >> { args ->
            assert Sort.by(Sort.Direction.ASC, "author") == args[0].sort
            new PageImpl<>([])
        }
    }

    def "Should ask for sorted by rss desc"() {
        given:
        BlogsPageRequestHandler handler = new BlogsPageRequestHandler()
        handler.setSort("rss", SortOrder.DESCENDING)

        when:
        handler.iterator(0, 10)

        then:
        1 * blogRepository.findAll((PageRequest) _) >> { args ->
            assert Sort.by(Sort.Direction.DESC, "rss") == args[0].getSort()
            new PageImpl<>([])
        }
    }

}
