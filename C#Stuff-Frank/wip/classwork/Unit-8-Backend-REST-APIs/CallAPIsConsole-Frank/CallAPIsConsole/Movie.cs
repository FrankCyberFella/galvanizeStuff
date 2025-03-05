using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

/*******************************************************************************
 * This is the data we are sending and receiving to/from the API server
 * 
 * We mjst be sure the names of teh data memebrs match the JSON property names
 * OR use [JsonPropertyName] annotation to associate them
 * 
 ******************************************************************************/

namespace moviesAPI.Models;

public class Movie
{
    [JsonPropertyName("movieId")]
    public int     MovieId     { get; set; }

    [JsonPropertyName("title")]
    public string  Title       { get; set; } = null!;

    [JsonPropertyName("releaseYear")]
    public int?    ReleaseYear { get; set; }

    [JsonPropertyName("director")]
    public string? Director    { get; set; }
}
