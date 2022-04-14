require 'Datavyu_API.rb'

begin
    #Create the output file
    out_file = File.expand_path("~/Desktop/activity.csv")
	out = File.new(out_file,'w')
    
    #Get the data from datavyu
    pos_sync = create_mutually_exclusive("pos_sync", "position", "position_parent")
    #setColumn("pos_sync", pos_sync)
    sync = getColumn("sync")

    for scell in sync.cells
        if scell.time == "4"
            for pcell in pos_sync.cells
                if pcell.onset >= scell.onset and pcell.offset <= scell.offset
                    out.write(pcell.onset.to_s + "," + pcell.offset.to_s + "," + pcell.position_pos + "," + pcell.position_parent_pos)
                    out.write("\n")
                end
            end
        end
    end
end
