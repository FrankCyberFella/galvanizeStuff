import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { MemberListComponent } from './components/member-list/member-list.component';

@Component({
  selector: 'app-root',
  imports: [MemberListComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'bookclub-frontend';
}
