require 'Datavyu_API.rb'

begin
    
    pos = new_column("position_parent","pos")
    pos_rel = new_column("position_parent_rel","pos")

    set_column(pos)
    set_column(pos_rel)
end
