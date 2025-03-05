package co.grandcircus.frankapiserverwithsql;

import org.springframework.data.jpa.repository.JpaRepository;

// Our repository is a sub class of JpaRepository
// We need to have all teh methods JpaRepository says we should have
// Spring Boot will use Dependency Injection to instantiate an object with
//        methods containing the default behavior required by JpaRepository
// We have tell JpaRepository the model class and the datatype of the Id of the model
//    (use the Java wrapper classes for the data type - primitive types not allowed)
public interface GamblerRepository extends JpaRepository<Gambler, Integer> {
}
