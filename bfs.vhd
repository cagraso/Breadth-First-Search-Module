 
-- Breadth First Search Module
------------------------------------------------------------------------------------------------------------------------------------------------------
-- Brief explanation of the module:

-- Breadth First Search module can store and traverse over a DAG graph having a user defined maximum node number. 
-- Module sends visited node info to the output port at every clock cycle. 
-- Latency of "traverse" start" signal to valid "visited" node info at the output port is 4 clock cycles. 
-- Adjacent nodes of a visited node is stored in a queue at every clock cycle.

-- Module holds graph information in node value map, node adjacency list and node adjacency number arrays. 
-- Each node in the graph is assigned to a specific index number. To access a node in the graph, assigned index values are used.
-- Adjacency list of each node is represented as 2D array containing values of adjacent nodes.
-- Module can store values of each node and adjacencies of each node at every clock cycle.

-- Module functional summary:
    -- Constructing graph with input node data.
    -- Traversing over the graph.
    -- Storing adjacent nodes in a queue.
    -- Sending results. 
------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity bfs is
    generic (   MAX_NODE_NUM    : integer range 0 to 64:=64;                -- maximum node number of the graph
                VALUE_WIDTH     : integer range 0 to 7:=7                   -- width of the value vector that represents the value of a node
            );
    Port    (   CLK         : in std_logic;                                 -- module clock signal
                RST         : in std_logic;                                 -- module reset signal
                DATA        : in std_logic_vector(VALUE_WIDTH-1 downto 0);  -- data input representing node values 
                NODE_IN     : in std_logic;                                 -- node data ready signal
                ADJ_IN      : in std_logic;                                 -- node adjacency data ready signal
                TRAVERSE    : in std_logic;                                 -- graph traversing start signal 
                VISITED     : out std_logic_vector(VALUE_WIDTH-1 downto 0); -- visited node value
                COMPLETE    : out std_logic                                 -- graph traversing completed signal
          );
end bfs;

architecture behavioral of bfs is

-- Node value map array that assigns a specific index for each node.
-- ( node values: 1 to MAX_NODE_NUM, node indices: 1 to MAX_NODE_NUM )  (index 0 and value 0 is not used) 
type node_val_map_array is array(0 to MAX_NODE_NUM) of integer range 0 to MAX_NODE_NUM; 
signal node_val_map: node_val_map_array; -- array mapping node values to indices

-- Node adjacency list array that holds adjacent node values of each node in the graph. 
-- ( node indices(first dimension): 1 - MAX_NODE_NUM, adjacent nodes(second dimension): 0 to MAX_NODE_NUM-2, adjacent node values: 1 to MAX_NODE_NUM ) 
type node_adj_array is array(0 to MAX_NODE_NUM, 0 to MAX_NODE_NUM-2) of integer range 0 to MAX_NODE_NUM; 
signal node_adj: node_adj_array; 

-- Node adjacency number array that holds number of adjacent nodes of each node in the graph.
type node_adj_num_array is array(0 to MAX_NODE_NUM) of integer range 0 to MAX_NODE_NUM-1; 
signal node_adj_num: node_adj_num_array; 


-- Graph node info store process signals
signal ind_1:           integer range 0 to MAX_NODE_NUM:=0; 
signal ind_2:           integer range 0 to MAX_NODE_NUM:=0; 
signal adj_ind:         integer range 0 to MAX_NODE_NUM-1:=0; 

-- Source, destination and visited nodes 
signal source_node:     integer range 0 to MAX_NODE_NUM;
signal target_node:     integer range 0 to MAX_NODE_NUM;
signal visited_node:    integer range 0 to MAX_NODE_NUM;       

-- Graph traverse controller process signals
signal cnt_state:       std_logic:='0';   
signal source_index:    integer range 0 to MAX_NODE_NUM;
signal traverse_en:     std_logic:='0';                         

-- Graph traverse process signals 
signal index:           integer range 0 to MAX_NODE_NUM;
signal adj_num:         integer range 0 to MAX_NODE_NUM-1;
signal q_ptr:           integer range 0 to MAX_NODE_NUM-1:=0;   
signal queue_0:         integer range 0 to MAX_NODE_NUM;   
signal tr_state:        std_logic_vector(1 downto 0):="00";
signal fill_queue:      std_logic:='0';                        
signal shift_queue:     std_logic:='0';                         

-- Queue array
type queue_array is array (0 to MAX_NODE_NUM-1) of integer range 0 to MAX_NODE_NUM; 
signal queue:           queue_array ;                           

begin

-- Receive graph data / Store graph data -- 
-- This process receives graph data as a sequence of node values and node adjacencies.
-- Master module (module that controls BFS module) sets node value on DATA port and asserts NODE_IN signal. 
-- At the next clock cycle, master module sets adjacent node value on DATA port and asserts ADJ_IN signal.
-- At every clock cycle , master module continues to send adjacent node values until the last adjacent node is sent.
store_graph:process(CLK) 
variable value:         integer range 0 to MAX_NODE_NUM;
begin

    if rising_edge(CLK) then
        
        if(RST='1') then
        
            -- Initializing node value, node adjacency list and node value map arrays
            for i in 0 to MAX_NODE_NUM loop
                
                node_adj_num(i) <= 0;
                node_val_map(i) <= 0;
                
                for j in 0 to MAX_NODE_NUM-2 loop
                    node_adj(i,j) <= 0;
                end loop;
                
            end loop; 
            
            ind_1       <= 0;
            ind_2       <= 0;
            adj_ind     <= 0;
            
        else
        
            -- Inserting node value data
            if(NODE_IN='1') then
                value               := to_integer(unsigned(DATA));
                node_val_map(value) <= ind_1;
                ind_1               <= ind_1 + 1;
                ind_2               <= ind_1;
                adj_ind             <= 0;
            end if;
            
            -- Inserting adjacency data
            if(ADJ_IN='1') then
                value                       := to_integer(unsigned(DATA));
                node_adj(ind_2,adj_ind)     <= value;
                node_adj_num(ind_2)         <= adj_ind + 1;
                adj_ind                     <= adj_ind + 1;
            end if;
        
        end if;        
    end if;
end process store_graph;


-- Controlling graph traverse  / Sending visited nodes to the output data port --
-- This process checks for the traverse start indicator.
-- Visited nodes are sent to the output port at every clock cycle.
graph_traverse_controller:process(CLK) 
begin

    if rising_edge(CLK) then
        
        if(RST='1') then
            cnt_state       <= '0';
            COMPLETE        <= '0';
            VISITED         <= std_logic_vector(to_unsigned(0,VALUE_WIDTH)); 
            traverse_en     <= '0'; 
            
        else
        
            case (cnt_state) is
            
                when '0' =>
                
                    traverse_en        <= '0';
                
                    -- Starting graph traverse
                    if(TRAVERSE='1') then
                        cnt_state   <= '1';
                        source_node <= to_integer(unsigned(DATA));
                        target_node <= to_integer(unsigned(DATA));
                    else
                        cnt_state   <= '0';
                    end if;
                    
                    COMPLETE        <= '0';
                    
                    VISITED         <= std_logic_vector(to_unsigned(0,VALUE_WIDTH));     
                
                when '1' =>
                    
                    traverse_en <= '1'; -- traverse process enable signal
                    
                    -- Checking if the destination is reached
                    if (visited_node=target_node) then
                        COMPLETE        <= '1'; -- traverse completed signal
                        cnt_state       <= '0';
                    else
                        COMPLETE        <= '0';
                        cnt_state       <= '1';
                    end if; 
                    
                    VISITED             <= std_logic_vector(to_unsigned(visited_node,VALUE_WIDTH));  -- visited node  
                    
                    source_index        <= node_val_map(source_node);                                -- index of the source node
                               
                
                when others=> null; 
            end case;
        
        end if;        
    end if;
end process graph_traverse_controller;


-- Traversing over the graph --
graph_traverse:process(CLK) 
variable current_node: integer range 0 to MAX_NODE_NUM; 
begin

    if rising_edge(CLK) then
        
        if(traverse_en='1') then
            
            -- Graph traverse state machine
            case (tr_state) is
            
                when "00" =>    -- queue source node 
                
                    -- Queue parameters for the first cycle
                    index           <= source_index;                            -- index of the source node
                    adj_num         <= node_adj_num(source_index);              -- adjacency number of the source node     
                    q_ptr           <= 0;                                       -- queue pointer / position of queue array index                                
                    fill_queue      <= '1';                                     -- fill queue / store adjacent nodes enable signal
                    shift_queue     <= '0';                                     -- shift queue disable signal
                    
                    queue_0         <= node_adj( source_index, 0 );             -- first adjacent node of the source node
                    
                    visited_node    <= 0;                                       -- visited node value
                    
                    tr_state        <= "01";                                    -- traverse state
                
                when "01" =>    -- first queue cycle
                
                    -- Updating parameters for the second queue cycle
                    index           <= node_val_map(queue_0);                   -- index of the first adjacent node of the source node
                    adj_num         <= node_adj_num(node_val_map(queue_0));     -- adjacency number of the first adjacent node
                    q_ptr           <= adj_num - 1;                             -- queue pointer / position of queue array index     
                    
                    fill_queue      <= '1';                                     -- fill queue / store adjacent nodes enable signal
                    shift_queue     <= '1';                                     -- shift queue enable signal
                    
                    visited_node    <= source_node;                             -- visited node value
                    
                    tr_state        <= "10";                                    -- traverse state
                    
                when "10" =>    -- shift queue for cycle synchronization
                    
                    -- Updating parameters for the third queue cycle
                    adj_num         <= 0;                                       -- adjacency number of the visited node
                    q_ptr           <= q_ptr + adj_num - 1 ;                    -- queue pointer / position of queue array index 
                    fill_queue      <= '0';                                     -- fill queue / store adjacent nodes disable signal
                    shift_queue     <= '1';                                     -- shift queue enable signal
                    
                    visited_node    <= queue(0);                                -- visited node value
                    
                    tr_state        <= "11";                                    -- traverse state
                    
                when "11" =>    -- get the first element in the queue and store adjacent nodes
                    
                    -- Update parameters for the queue process
                    index           <= node_val_map(queue(0));                  -- index of the visited node
                    adj_num         <= node_adj_num(node_val_map(queue(0)));    -- adjacency number of the visited node
                    q_ptr           <= q_ptr + adj_num - 1 ;                    -- queue pointer / position of queue array index 
                    
                    fill_queue      <= '1';                                     -- fill queue / store adjacent nodes enable signal
                    shift_queue     <= '1';                                     -- shift queue enable signal
                    
                    visited_node    <= queue(0);                                -- visited node value
                    
                    tr_state        <= "11";                                    -- traverse state
           
                when others => null;
            
            end case;
        
        else
            fill_queue      <= '0';
            shift_queue     <= '0';
            visited_node    <= 0;
            tr_state        <= "00";
        
        end if;
    
    end if;
    
end process graph_traverse;


--  Storing adjacent nodes in the queue / Shifting the queue --
queue_nodes:process(CLK) 
begin

    if rising_edge(CLK) then
        
        if(fill_queue='1') then

            -- Queue adjacent nodes of the visited node
            for i in 0 to MAX_NODE_NUM-2 loop 
                queue(q_ptr + i)  <= node_adj( index, i );   
            end loop;

        end if;  
        
        if(shift_queue='1') then

            -- Shift queue
            for i in 0 to MAX_NODE_NUM-2 loop
               queue(i) <= queue(i+1); 
            end loop; 

        end if;      
    
    end if;
end process queue_nodes;


end behavioral;
