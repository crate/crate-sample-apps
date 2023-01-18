package io.crate.spring.jdbc.samples;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.crate.spring.jdbc.samples.dao.ImagesDao;
import io.crate.spring.jdbc.samples.model.Image;

@RestController
public class ImagesController {

    private static final Log logger = LogFactory.getLog(ImagesController.class);

    @Autowired
    private ImagesDao dao;

    @Autowired
    private ImagesSerializer imagesSerializer;

    @GetMapping("/images")
    public List<Map<String, Object>> getImages() {
        logger.debug("Searching for images in database");
        var images = this.dao.getImages();
        logger.info("Found " + images.size() + " in database");
        return this.convertToStdResult(images);
    }

    @RequestMapping(value = "/image/{digest}", method = RequestMethod.GET)
    public void getImageAsByteArray(HttpServletResponse response, @PathVariable String digest) throws IOException {
        logger.debug("Downloading image with digest " + digest);
        if (dao.imageExists(digest)) {
            InputStream in = dao.getImageAsInputStream(digest);
            response.setContentType(MediaType.IMAGE_GIF_VALUE);
            IOUtils.copy(in, response.getOutputStream());
        } else {
            throw new ImageNotExistsException(digest);
        }
    }

    @DeleteMapping("/image/{digest}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteImage(@PathVariable String digest) {
        logger.debug("Deleting image with digest " + digest);
        if (this.dao.imageExists(digest)) {
            this.dao.deleteImage(digest);
        } else {
            throw new ImageNotExistsException(digest);
        }
    }

    @PostMapping("/images")
    public Map<String, String> insertImage(@RequestBody(required=false) Map<String, Object> imageProps, HttpServletResponse response) {
        logger.debug("Inserting image into database");
        if (imageProps == null) {
            throw new ArgumentRequiredException("Request body is required");
        } else if (!imageProps.containsKey("blob")) {
            throw new ArgumentRequiredException("Argument \"blob\" is required");
        }

        var decodedBytes = Base64.getDecoder().decode((String) imageProps.get("blob"));
        var digest = DigestUtils.sha1Hex(decodedBytes);

        var responseMap = dao.insertImage(digest, decodedBytes);
        response.setStatus(Integer.parseInt(responseMap.get("status")));
        return responseMap;
    }

    private List<Map<String, Object>> convertToStdResult(final List<Image> images) {
        var result = new ArrayList<Map<String, Object>>();
        for (var image : images) {
            result.add(imagesSerializer.serialize(image));
        }
        return result;
    }
}