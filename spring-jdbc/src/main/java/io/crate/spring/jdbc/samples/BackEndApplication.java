package io.crate.spring.jdbc.samples;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import io.crate.spring.jdbc.samples.dao.JDBCDBObjectsFactory;
import io.crate.spring.jdbc.samples.dao.PostgresJDBCDBObjectsFactory;

@SpringBootApplication
public class BackEndApplication {

    public static void main(String[] args) {
        SpringApplication.run(BackEndApplication.class, args);
    }

    @Bean
    public BlogPostSerializer createPostsSerializer() {
        return new BlogPostSerializer();
    }

    @Bean
    public BlogPostDeserializer createPostsDeserializer() {
        return new BlogPostDeserializer();
    }

    @Bean
    public JDBCDBObjectsFactory createObjectsFactory() {
        return new PostgresJDBCDBObjectsFactory();
    }

    @Bean
    public ImagesSerializer createImagesSerializer() {
        return new ImagesSerializer();
    }

    @Bean
    public WebMvcConfigurer corsConfigurer() {
        return new WebMvcConfigurer() {
            @Override
            public void addCorsMappings(CorsRegistry registry) {
                registry.addMapping("/**").allowedMethods("GET", "POST", "DELETE", "PUT");
            }
        };
    }
}
