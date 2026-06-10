import stdlib.libc;

struct AllocSlab {
  prev: AllocSlab*;
  nextFree: iptr;
  size: iptr;
  mem: i8[0];
}

struct Allocator {
  current: AllocSlab*;
}

const slab_size = 1 << 16;

func newSlab(allocator: Allocator*, size: iptr) -> [i8] {
  let new = calloc(1, sizeof(AllocSlab) + size as uptr) as AllocSlab*;
  new->prev = allocator->current;
  new->size = size;
  new->nextFree = size;
  allocator->current = new;

  return new->mem[:size];
}

func reallocSlab(allocator: Allocator*, size: iptr) -> [i8] {
  allocator->current = realloc(allocator->current, sizeof(AllocSlab) + size as uptr) as AllocSlab*;
  allocator->current->size = size;
  allocator->current->nextFree = size;
  return allocator->current->mem[:size];
}


// TODO: what if size > slab_size
func alloc(allocator: Allocator*, size: iptr) -> void* {
  if (allocator->current == null) {
    allocator->current = calloc(1, slab_size);
  }

  let cur = allocator->current;
  if (cur->nextFree + sizeof(AllocSlab) + size >= cur->size) {
    let new = calloc(1, slab_size) as AllocSlab*;
    new->prev = cur;
    new->size = slab_size;
    allocator->current = new;
    cur = new;
  }

  let result = &cur->mem[cur->nextFree];
  cur->nextFree += size;
  return result;
}

func freeAll(allocator: Allocator*) {
  let cur = allocator->current;
  while (cur != null) {
    let next = cur->prev;
    free(cur);
    cur = next;
  }
}
