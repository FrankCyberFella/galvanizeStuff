using System;
using System.Collections.Generic;

namespace places_api.Models;

public partial class Place
{
    public int Id { get; set; }

    public string? Name { get; set; }

    public bool? FirstTime { get; set; }
}
