import { Routes }               from '@angular/router';
import { HomeComponent }        from './home/home.component';
import { GamblerListComponent } from './gambler-list/gambler-list.component';
import { AddGamblerComponent }  from './add-gambler/add-gambler.component';

export const routes: Routes = [
    {path: '', redirectTo: '/home', pathMatch: 'full' }  ,
    {path: 'home',       component: HomeComponent}       ,
    {path: 'gamblers',   component: GamblerListComponent} ,
    {path: 'addgambler',   component: AddGamblerComponent} ,
   
];
