import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { CommonModule } from '@angular/common';
import { SampleFormComponent } from '../sample-form/sample-form.component';
import { StudentListComponent } from '../student-list/student-list.component';

const routes: Routes = [
  { path: '', redirectTo: '/students', pathMatch: 'full' },
  { path: 'students',    component: StudentListComponent },
  { path: 'contactInfo', component: SampleFormComponent }
  ]

@NgModule({
  declarations: [],
  imports: [CommonModule, RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule {

 }
