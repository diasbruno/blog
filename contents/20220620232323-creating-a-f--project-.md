nix-shell -p fsharp

# so it can find labraries and dependencies
export DOTNET_ROOT=/nix/store/vl482bnf0ryzz0yg1lhfy5imc8rv04rm-dotnet-sdk-6.0.202

dotnet new sln -n {NAME}

dotnet new classlib -n {LIBRARY_NAME} -lang {LANG}

dotnet sln add classlib

dotnet new console -n {APP_NAME} -lang {LANG}

dotnet sln add console

cd {APP_NAME}

dotnet add reference ../{LIBRARY_NAME}

cd ..

dotnet build

dotnet run --project {APP_NAME}

dotnet add package FSharp.ORM
