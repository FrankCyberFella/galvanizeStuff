using Microsoft.EntityFrameworkCore;

namespace GamblerAPI.Model
{
    /***********************************************************************************
     * 
     * The DbContext class/Object established Entity Framework features for the table
     *         
     * You define a subclass of DbContext ti add information about your table 
     * 
     * partial is added to subclass to indicate it is only a partial decription
     * of what Entity Framework needs to work
     * 
     * partial means your subclass will be essentially merged into Entity Framework
     * 
     **********************************************************************************/ 


    public partial class GamblerDbContext : DbContext
    {
        // Define constructor to take options passed when instatiated by application
        //        and pass those options to base (DbContext) class constructor
        public GamblerDbContext(DbContextOptions<GamblerDbContext> options)
            : base(options) { }

        // Define property to represent the collection of data in the data source
        // Our data source is called Gamblers
        public virtual DbSet<Gambler> Gamblers { get; set; }

        // Define table and columns to be accessed using Entity Framework
        // ModelBuilder will create Entity Framework structures to represent
        //     the table you will be accessing
        //
        // ModelBuilder uses the "Builder Design Pattern" to create and initialize the object
        //      (instead of constructors)
        // "Builder Design Pattern" allow you to initialize an object one step at a time
        //          instead of with constructors
        //
        // Suppose you had an object with 25 data members, but some were optional
        // You could have a constructor that takes all 25 member values,
        //     but then you would have to others that took other combinations of
        //     of less that 25 values.
        //  It's possible you would need 25 constructors or more.
        //
        //  The "Builder Design Pattern" makes it much easier to initialize objects
        //      with many data members.
        //
        //  To use the "Builder Design Pattern":
        // 
        //      Define a builder class for the object
        //           with one method to initialize on data member
        //
        //      When all individual data members you want to initialize have
        //           been assigned values
        //
        //      Call the "build()' method of teh builder to instantiate the object
        //           and assign the values to object you have been giving the building
        //
        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            // Create an Entity Framework entity to represent our Gambler table
            //      with a Gambler object
            modelBuilder.Entity<Gambler>(entity =>
            {
                entity.ToTable("gambler");  // Tell it the table name

                // Tell it about each column in the table
                entity.Property(e => e.Id)      // The Gambler class member called "Id"
                    .HasColumnName("id")        // Relates to table column called "id"
                    .IsRequired()               // Cannot be nulll
                    .HasColumnType("smallint"); // Data type of the column in the table

                entity.Property(e => e.Name)
                    .IsRequired()
                    .HasColumnName("gambler_name")
                    .HasColumnType("text");
                
                // IsRequired() is not specified for address because it allows nulls
                entity.Property(e => e.Address)
                    .HasColumnName("address")
                    .HasColumnType("text")
                    .HasDefaultValue("unknown");  // Since address allows null we can specify a default valte

                entity.Property(e => e.BirthDate)
                      .IsRequired()
                      .HasColumnName("birth_date")
                      .HasColumnType("date");

                entity.Property(e => e.Salary)
                    .IsRequired()
                    .HasColumnName("monthly_salary")
                    .HasColumnType("decimal(9,2)");

                // Once we have told Entity Framework about all the columns,
                // caller the "build" method of the Builder to instatiate 
                // the class associated in EF with our Gambler class
                OnModelCreatingPartial(modelBuilder);
            });  // End of modelBuilder.entity()
        }  // End of onModelCreating()

        // method called to instantiate the objects in EF
        partial void OnModelCreatingPartial(ModelBuilder modelBuilder);
        
    } // End of class
} // End of namespace
