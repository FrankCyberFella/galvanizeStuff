using System;
using System.Collections.Generic;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class GildedRoseTest
{
    [Test]
    public void Foo()
    {
        var items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
        var app = new GildedRose(items);
        app.UpdateQuality();
        Assert.That(items[0].Name, Is.EqualTo("foo"));
    }
    [Test]
    public void NormalItem1() {
         var items = new List<Item> { new Item { Name = "Exlixir of the Mongoose", SellIn = 4, Quality = 6 } };
         var app   = new GildedRose(items);
         app.UpdateQuality();
         Assert.That(items[0].SellIn, Is.EqualTo(3));
         
    }
}