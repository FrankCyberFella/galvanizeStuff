package model

public class Item {

  var _id : String as Id
  var _name : String as Name
  var _description : String as Description
  var _createdDate : Date as CreatedDate
  var _createdBy : String as CreatedBy
  var _updatedDate : Date as UpdatedDate
  var _updatedBy : String as UpdatedBy

  construct(id : String, name : String, description : String, createdDate : Date, createdBy : String, updatedDate : Date, updatedBy : String) {
    this._id = id
    this._description = description
    this._name = name
    this._createdDate = createdDate
    this._createdBy = createdBy
    this._updatedDate = updatedDate
    this._updatedBy = updatedBy
  }

  construct(id : String, name : String, description : String, createdDate : Date, createdBy : String) {
    this._id = id
    this._description = description
    this._name = name
    this._createdDate = createdDate
    this._createdBy = createdBy
  }

  construct() {
  }


  override public function toString() : String {
    return "Item{" +
        "_id='" + _id + '\'' +
        ", _name='" + _name + '\'' +
        ", _description='" + _description + '\'' +
        ", _createdDate=" + _createdDate +
        ", _createdBy='" + _createdBy + '\'' +
        ", _updatedDate=" + _updatedDate +
        ", _updatedBy='" + _updatedBy + '\'' +
        '}';
  }

  override public function equals(o : Object) : boolean {
    var item : Item = o as Item;
    if ( this.Id != item.Id) return false
    return true;
  }

  override public function hashCode() : int {
    return _id != null ? _id.hashCode() : 0;
  }
}