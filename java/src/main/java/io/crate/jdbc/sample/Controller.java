package io.crate.jdbc.sample;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.http.client.methods.CloseableHttpResponse;
import spark.Response;
import spark.utils.IOUtils;

import java.sql.SQLException;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import static spark.Spark.*;

public class Controller {

    private final Gson gson = new GsonBuilder().serializeNulls().create();

    private static final int INTERNAL_ERROR = 500;
    private static final int BAD_REQUEST = 400;
    private static final int NOT_FOUND = 404;

    private static final int NO_CONTENT = 204;
    private static final int CREATED = 201;
    private static final int OK = 200;

    public Controller(final DataProvider model) {

        before(((request, response) -> {
            response.header("Access-Control-Allow-Origin", "*");
            response.header("Access-Control-Request-Method", "*");
            response.header("Access-Control-Allow-Headers", "*");
        }));

        get("/posts", (request, response) ->
                model.getPosts(), gson::toJson);

        post("/posts", (request, response) -> {
            String body = request.body();
            if (body.isEmpty()) {
                return argumentRequired(response, "Request \"body\" is required");
            }

            Map post = gson.fromJson(body, Map.class);
            if (!post.containsKey("text")) {
                return argumentRequired(response, "Request \"text\" is required");
            }

            Map user = (Map) post.get("user");
            if (!user.containsKey("location")) {
                return argumentRequired(response, "Request \"location\" is required");
            }
            response.status(CREATED);
            return model.insertPost(post);
        }, gson::toJson);

        get("/post/:id", (request, response) -> {
            String id = request.params(":id");
            Map post = model.getPost(id);
            if (post.isEmpty()) {
                return notFound(response, (String.format("Post with id=\"%s\" not found", id)));
            }
            response.status(OK);
            return post;
        }, gson::toJson);

        put("/post/:id", (request, response) -> {
            String body = request.body();
            if (body.isEmpty()) {
                return argumentRequired(response, "Request \"body\" is required");
            }

            Map post = gson.fromJson(body, Map.class);
            if (!post.containsKey("text")) {
                return argumentRequired(response, "Request \"text\" is required");
            }

            String id = request.params(":id");
            Map updatePost = model.updatePost(id, (String) post.get("text"));
            if (updatePost.isEmpty()) {
                return notFound(response, (String.format("Post with id=\"%s\" not found", id)));
            }
            response.status(OK);
            return updatePost;
        }, gson::toJson);

        delete("/post/:id", (request, response) -> {
            String id = request.params(":id");
            if (model.deletePost(id)) {
                response.status(NO_CONTENT);
                JsonObject responseJson = new JsonObject();
                responseJson.addProperty("success", true);
                return responseJson;
            } else {
                return notFound(response, String.format("Post with id=\"%s\" not found", id));
            }
        }, gson::toJson);

        put("/post/:id/like", (request, response) -> {
            String id = request.params(":id");
            Map post = model.incrementLike(id);
            if (post.isEmpty()) {
                return notFound(response, (String.format("Post with id=\"%s\" not found", id)));
            }
            response.status(OK);
            return post;
        }, gson::toJson);

        get("/images", (request, response) -> model.getBlobs(), gson::toJson);

        post("/images", (request, response) -> {
            String body = request.body();
            if (body.isEmpty()) {
                return argumentRequired(response, "Request \"body\" is required");
            }

            Map blobMap = gson.fromJson(body, Map.class);
            if (!blobMap.containsKey("blob")) {
                return argumentRequired(response, "Request \"blob\" is required");
            }

            byte[] decoded = Base64.getDecoder().decode((String) blobMap.get("blob"));
            Map<String, String> responseMap = model.insertBlob(DigestUtils.shaHex(decoded), decoded);

            response.status(Integer.parseInt(responseMap.get("status")));
            return responseMap;
        }, gson::toJson);

        get("/image/:digest", (request, response) -> {
            String digest = request.params(":digest");
            if (model.blobExists(digest)) {
                CloseableHttpResponse closeableHttpResponse = model.getBlob(digest);
                Arrays.stream(closeableHttpResponse.getAllHeaders()).forEach(header ->
                                response.header(header.getName(), header.getValue())
                );

                response.status(closeableHttpResponse.getStatusLine().getStatusCode());
                response.header("Content-Type", "image/gif");
                response.body(IOUtils.toString(closeableHttpResponse.getEntity().getContent()));
                return response;
            } else {
                return gson.toJson(
                        notFound(response, String.format("Image with digest=\"%s\" not found", digest))
                );
            }
        });

        delete("/image/:digest", (request, response) -> {
            String digest = request.params(":digest");
            if (model.blobExists(digest)) {
                CloseableHttpResponse closeableHttpResponse = model.deleteBlob(digest);
                int status = closeableHttpResponse.getStatusLine().getStatusCode();
                response.status(status);
                JsonObject responseJson = new JsonObject();
                responseJson.addProperty("success", status == OK);
                return responseJson;
            } else {
                return notFound(response, String.format("Image with digest=\"%s\" not found", digest));
            }
        }, gson::toJson);

        exception(SQLException.class, (e, request, response) -> {
            response.status(INTERNAL_ERROR);
            response.body(e.getLocalizedMessage());
        });
    }

    private Map<String, Object> notFound(Response response, String msg) {
        return errorResponse(response, msg, NOT_FOUND);
    }

    private Map<String, Object> argumentRequired(Response response, String msg) {
        return errorResponse(response, msg, BAD_REQUEST);
    }

    private Map<String, Object> errorResponse(Response response, String msg, int code) {
        response.status(code);
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("status", code);
        responseMap.put("error", msg);
        return responseMap;
    }

}
