#!/usr/bin/env bash
echo "Start Transient Loop..."
cp edges.loop.1 edges;
./pvr 4;
./pvr 1;
./pvr 2;
./pvr 3;
cp table.4 start.table.4;
cp table.1 start.table.1;
cp table.3 start.table.3;
cp table.2 start.table.2;
echo "Network Converged for All Routes to Node 5, table files saved to start.table.*"
cp edges.loop.2 edges;
./pvr 4;
./pvr 1;
./pvr 2;
cp table.4 loop.table.4;
cp table.1 loop.table.1;
cp table.2 loop.table.2;
echo "Transient Loop formed on Nodes 1, 2, 3 for Destination Node 5, changed table files saved to loop.table.*"
./pvr 1;
./pvr 3;
./pvr 2;
cp table.1 resolved.table.1;
cp table.3 resolved.table.3;
cp table.2 resolved.table.2;
echo "Transient Loop resolved for Destination Node 5, changed table files saved to resolve.table.*"