#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <limits>
#include <tuple>

using namespace std;

// Função utilitária para imprimir uma matriz de forma formatada.
void printMatrix(const vector<vector<int>>& matrix) {
    for (const auto& row : matrix) {
        for (int val : row) {
            cout << setw(6) << val;
        }
        cout << endl;
    }
}

/**
 * @brief Encontra uma Solução Básica Factível pelo Método do Canto Noroeste.
 */
double northwestCorner(
    const vector<vector<int>>& costs,
    vector<int> supply,
    vector<int> demand,
    vector<vector<int>>& allocation) {

    int i = 0, j = 0;
    int num_suppliers = supply.size();
    int num_consumers = demand.size();

    while (i < num_suppliers && j < num_consumers) {
        int quantity = min(supply[i], demand[j]);
        allocation[i][j] = quantity;
        supply[i] -= quantity;
        demand[j] -= quantity;

        if (supply[i] == 0) i++;
        else j++;
    }

    double total_cost = 0.0;
    for (int r = 0; r < num_suppliers; ++r) {
        for (int c = 0; c < num_consumers; ++c) {
            if (allocation[r][c] > 0) {
                total_cost += allocation[r][c] * costs[r][c];
            }
        }
    }
    return total_cost;
}

/**
 * @brief Encontra uma Solução Básica Factível pelo Método do Custo Mínimo.
 */
double minimumCost(
    const vector<vector<int>>& costs,
    vector<int> supply,
    vector<int> demand,
    vector<vector<int>>& allocation) {

    int num_suppliers = supply.size();
    int num_consumers = demand.size();
    vector<bool> supply_done(num_suppliers, false);
    vector<bool> demand_done(num_consumers, false);
    
    int cells_to_fill = num_suppliers * num_consumers;
    for (int k = 0; k < cells_to_fill; ++k) {
        int min_cost = numeric_limits<int>::max();
        int best_row = -1, best_col = -1;

        for (int i = 0; i < num_suppliers; ++i) {
            if (!supply_done[i]) {
                for (int j = 0; j < num_consumers; ++j) {
                    if (!demand_done[j] && costs[i][j] < min_cost) {
                        min_cost = costs[i][j];
                        best_row = i;
                        best_col = j;
                    }
                }
            }
        }
        
        if (best_row == -1) break;

        int quantity = min(supply[best_row], demand[best_col]);
        allocation[best_row][best_col] = quantity;
        supply[best_row] -= quantity;
        demand[best_col] -= quantity;
        
        if (supply[best_row] == 0) supply_done[best_row] = true;
        if (demand[best_col] == 0) demand_done[best_col] = true;
    }

    double total_cost = 0.0;
    for (int r = 0; r < num_suppliers; ++r) {
        for (int c = 0; c < num_consumers; ++c) {
            if (allocation[r][c] > 0) {
                total_cost += allocation[r][c] * costs[r][c];
            }
        }
    }
    return total_cost;
}

/**
 * @brief Verifica a otimalidade da solução usando o método dos multiplicadores (MODI).
 *        NOTA: Neste caso, delta NEGATIVO indica que a solução É ótima.
 */
bool checkOptimalityMODI(const vector<vector<int>>& costs, const vector<vector<int>>& allocation) {
    int m = costs.size();        // número de fornecedores
    int n = costs[0].size();     // número de consumidores

    vector<int> u(m, numeric_limits<int>::max()); // u[i]
    vector<int> v(n, numeric_limits<int>::max()); // v[j]

    u[0] = 0; // arbitrário

    // Obter todas as células básicas (com alocação)
    vector<tuple<int, int>> basic_cells;
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (allocation[i][j] > 0) {
                basic_cells.emplace_back(i, j);
            }
        }
    }

    // Calcular u[i] e v[j]
    bool updated;
    do {
        updated = false;
        for (auto& cell : basic_cells) {
            int i, j;
            tie(i, j) = cell;

            if (u[i] != numeric_limits<int>::max() && v[j] == numeric_limits<int>::max()) {
                v[j] = costs[i][j] - u[i];
                updated = true;
            } else if (v[j] != numeric_limits<int>::max() && u[i] == numeric_limits<int>::max()) {
                u[i] = costs[i][j] - v[j];
                updated = true;
            }
        }
    } while (updated);

    // Verificação de otimalidade com delta NEGATIVO indicando ótimo
    bool isOptimal = true;
    cout << "\nMatriz de Custo Reduzido (delta):" << endl;
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (allocation[i][j] == 0) {
                int delta = costs[i][j] - (u[i] + v[j]);
                cout << setw(6) << delta;
                if (delta >= 0) {
                    isOptimal = false;
                }
            } else {
                cout << setw(6) << "X";
            }
        }
        cout << endl;
    }

    return isOptimal;
}

/**
 * @brief Encontra uma Solução Básica Factível pelo Método de Aproximação de Vogel.
 */
double vogelsApproximation(
    const vector<vector<int>>& costs,
    vector<int> supply,
    vector<int> demand,
    vector<vector<int>>& allocation) {
    
    int num_suppliers = supply.size();
    int num_consumers = demand.size();
    vector<bool> supply_done(num_suppliers, false);
    vector<bool> demand_done(num_consumers, false);
    int allocations_made = 0;

    while(allocations_made < num_suppliers + num_consumers -1) {
        vector<int> row_penalty;
        vector<int> col_penalty;

        for(int i = 0; i < num_suppliers; i++){
            if(supply_done[i]) {
                row_penalty.push_back(-1);
                continue;
            }
            int min1 = numeric_limits<int>::max(), min2 = numeric_limits<int>::max();
            for(int j = 0; j < num_consumers; j++){
                if(!demand_done[j]){
                    if(costs[i][j] < min1){
                        min2 = min1;
                        min1 = costs[i][j];
                    } else if(costs[i][j] < min2){
                        min2 = costs[i][j];
                    }
                }
            }
            row_penalty.push_back(min2 == numeric_limits<int>::max() ? min1 : min2 - min1);
        }

        for(int j = 0; j < num_consumers; j++){
            if(demand_done[j]) {
                col_penalty.push_back(-1);
                continue;
            }
            int min1 = numeric_limits<int>::max(), min2 = numeric_limits<int>::max();
            for(int i = 0; i < num_suppliers; i++){
                if(!supply_done[i]){
                    if(costs[i][j] < min1){
                        min2 = min1;
                        min1 = costs[i][j];
                    } else if(costs[i][j] < min2){
                        min2 = costs[i][j];
                    }
                }
            }
            col_penalty.push_back(min2 == numeric_limits<int>::max() ? min1 : min2 - min1);
        }

        int max_penalty = -1;
        int penalty_row = -1, penalty_col = -1;
        bool is_row_penalty = false;

        for(int i=0; i<num_suppliers; i++) {
            if(row_penalty[i] > max_penalty) {
                max_penalty = row_penalty[i];
                penalty_row = i;
                is_row_penalty = true;
            }
        }
        for(int j=0; j<num_consumers; j++) {
            if(col_penalty[j] > max_penalty) {
                max_penalty = col_penalty[j];
                penalty_col = j;
                is_row_penalty = false;
            }
        }

        if(max_penalty == -1) break;

        int alloc_row = -1, alloc_col = -1;
        int min_cost = numeric_limits<int>::max();

        if(is_row_penalty){
            for(int j=0; j<num_consumers; j++){
                if(!demand_done[j] && costs[penalty_row][j] < min_cost){
                    min_cost = costs[penalty_row][j];
                    alloc_col = j;
                }
            }
            alloc_row = penalty_row;
        } else {
            for(int i=0; i<num_suppliers; i++){
                if(!supply_done[i] && costs[i][penalty_col] < min_cost){
                    min_cost = costs[i][penalty_col];
                    alloc_row = i;
                }
            }
            alloc_col = penalty_col;
        }

        int quantity = min(supply[alloc_row], demand[alloc_col]);
        allocation[alloc_row][alloc_col] = quantity;
        supply[alloc_row] -= quantity;
        demand[alloc_col] -= quantity;

        if(supply[alloc_row] == 0) supply_done[alloc_row] = true;
        if(demand[alloc_col] == 0) demand_done[alloc_col] = true;
        allocations_made++;
    }

    double total_cost = 0.0;
    for (int r = 0; r < num_suppliers; ++r) {
        for (int c = 0; c < num_consumers; ++c) {
            if (allocation[r][c] > 0) {
                total_cost += allocation[r][c] * costs[r][c];
            }
        }
    }
    return total_cost;
}


int main() {
    vector<vector<int>> costs = {
        {12, 22, 30},
        {18, 24, 32},
        {22, 15, 34}
    };
    vector<int> supply = {100, 140, 160};
    vector<int> demand = {120, 130, 150};

    cout << "==================================================" << endl;
    cout << "      METODO DO CANTO NOROESTE" << endl;
    cout << "==================================================" << endl;
    
    vector<vector<int>> allocationNW(supply.size(), vector<int>(demand.size(), 0));
    double total_cost_nw = northwestCorner(costs, supply, demand, allocationNW);
    
    cout << "\nMatriz de Alocacao Resultante:" << endl;
    printMatrix(allocationNW);
    cout << "\nCusto Total de Transporte: " << total_cost_nw << endl;


    cout << "\n\n==================================================" << endl;
    cout << "      METODO DO CUSTO MINIMO" << endl;
    cout << "==================================================" << endl;

    vector<vector<int>> allocationMC(supply.size(), vector<int>(demand.size(), 0));
    double total_cost_mc = minimumCost(costs, supply, demand, allocationMC);

    cout << "\nMatriz de Alocacao Resultante:" << endl;
    printMatrix(allocationMC);
    cout << "\nCusto Total de Transporte: " << total_cost_mc << endl;

    cout << "\n\nTeste de Otimalidade (MODI) para o Metodo do Custo Minimo:" << endl;
    bool isOptimal = checkOptimalityMODI(costs, allocationMC);
    if (isOptimal) {
        cout << "\nA solução É ÓTIMA (todos os deltas < 0)." << endl;
    } else {
        cout << "\nA solução NÃO é ótima (existe delta >= 0)." << endl;
    }


    cout << "\n\n==================================================" << endl;
    cout << "      METODO DE APROXIMACAO DE VOGEL" << endl;
    cout << "==================================================" << endl;

    vector<vector<int>> allocationVAM(supply.size(), vector<int>(demand.size(), 0));
    double total_cost_vam = vogelsApproximation(costs, supply, demand, allocationVAM);

    cout << "\nMatriz de Alocacao Resultante:" << endl;
    printMatrix(allocationVAM);
    cout << "\nCusto Total de Transporte: " << total_cost_vam << endl;

    return 0;
}
