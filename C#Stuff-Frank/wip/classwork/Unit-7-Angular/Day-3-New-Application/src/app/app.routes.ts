import { Routes } from '@angular/router';
import { StudentListComponent } from './student-list/student-list.component';
import { SampleFormComponent } from './sample-form/sample-form.component';

export const routes: Routes = [
    {path: '', redirectTo: '/home', pathMatch: 'full' },
    {path: 'students',    component: StudentListComponent },
    {path: 'contactInfo', component: SampleFormComponent}
];
