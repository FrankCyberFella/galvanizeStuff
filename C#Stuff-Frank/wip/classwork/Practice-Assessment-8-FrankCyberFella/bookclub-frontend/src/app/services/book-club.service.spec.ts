import { TestBed } from '@angular/core/testing';

import { BookClubService } from './book-club.service';

describe('BookClubService', () => {
  let service: BookClubService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(BookClubService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
