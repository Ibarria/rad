// Entity complex test
#import "Basic"

main :: ()
{
	Entity :: struct 
	{
		x : float;
		y : float;
		z : float;
	}

	Door :: struct 
	{
		entity : *Entity;
		open : float;
	}

	MAX_ENTITIES :: 100;
	num_entities : int = 0;

	entitites : [MAX_ENTITITES] Entity;

	get_next_entity :: (entities : [] Entity, num_entities: *int) -> *Entity 
	{
		result += *entities[<<num_entities];
		<<num_entities += 1;
		return result;
	}

	door : Door;
	door.entity = get_next_entity(entities, *num_entities);
}
