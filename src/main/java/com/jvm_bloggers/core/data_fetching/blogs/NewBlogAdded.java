package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.utils.JvmBloggersEvent;
import com.jvm_bloggers.entities.blog.Blog;
import lombok.Value;

@Value
@JvmBloggersEvent
public class NewBlogAdded {

    Blog newBlog;
}
