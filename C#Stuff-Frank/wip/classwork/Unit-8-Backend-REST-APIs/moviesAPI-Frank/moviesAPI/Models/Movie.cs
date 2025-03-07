﻿using System;
using System.Collections.Generic;

namespace moviesAPI.Models;

public partial class Movie
{
    public int     MovieId     { get; set; }

    public string  Title       { get; set; } = null!;

    public int?    ReleaseYear { get; set; }

    public string? Director    { get; set; }
}
