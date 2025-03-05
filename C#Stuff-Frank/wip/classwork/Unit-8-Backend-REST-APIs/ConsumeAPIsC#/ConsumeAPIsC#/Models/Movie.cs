using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace moviesAPI.Models;

public partial class Movie
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
