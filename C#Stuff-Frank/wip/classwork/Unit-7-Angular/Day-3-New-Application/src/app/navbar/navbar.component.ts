import { Component }    from '@angular/core';
import { RouterModule } from '@angular/router';

@Component({
  selector: 'app-navbar', // What is used in the HTML to reference teh component
  standalone: true,
  imports: [RouterModule],
  templateUrl: './navbar.component.html',
  styleUrl: './navbar.component.css'
})
export class NavbarComponent {

}
